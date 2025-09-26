# Qué hace:
# Tokeniza y parsea expresiones aritméticas con +, -, *, /, ^, paréntesis, variables y números.
# Convierte a notación infija, prefija (polaca) y posfija.
# Genera código intermedio: Código P (máquina de pila), Triplos y Cuádruplos.
# Interfaz gráfica con tkinter/ttk (sin dependencias externas).

import re
import sys
import tkinter as tk
from tkinter import ttk, messagebox
from tkinter.scrolledtext import ScrolledText
from collections import namedtuple

APP_TITLE = "Conversor de Expresiones/ISC"
VERSION = "1.0"

# =======================
#   Núcleo de compilación
# =======================

# ---- Definiciones de token ----
# Usamos namedtuple para representar tokens con tipo y valor.
Token = namedtuple("Token", "type value")
TT_NUM   = "NUM"     # números literales (p.ej., 12, 3.14)
TT_ID    = "ID"      # identificadores (variables) (p.ej., x, total_1)
TT_OP    = "OP"      # operador (+, -, *, /, ^, UMINUS)
TT_LP    = "LPAREN"  # '('
TT_RP    = "RPAREN"  # ')'

# ---- Tabla de operadores ----
# prec: precedencia (mayor = más fuerte)
# assoc: asociatividad ("left" o "right")
# arity: aridad (1 para UMINUS; 2 para el resto)
OP_INFO = {
    "^": {"prec": 4, "assoc": "right", "arity": 2},
    "*": {"prec": 3, "assoc": "left",  "arity": 2},
    "/": {"prec": 3, "assoc": "left",  "arity": 2},
    "+": {"prec": 2, "assoc": "left",  "arity": 2},
    "-": {"prec": 2, "assoc": "left",  "arity": 2},
    "UMINUS": {"prec": 5, "assoc": "right", "arity": 1},  # menos unario (negación)
}

def tokenize(expr: str):
    """
    Convierte una cadena de expresión en una lista de tokens.
    Reglas:
      - Números: enteros o flotantes (e.g., 12, 3.5)
      - IDs: [A-Za-z_] seguido de [A-Za-z0-9_]*
      - Paréntesis: ( )
      - Operadores: + - * / ^ y UMINUS (detectado por contexto)
    """
    expr = expr.strip()
    tokens = []
    i = 0
    prev_type = None  # Se usa para detectar si '-' es unario (al inicio, tras '(' o tras otro operador)

    while i < len(expr):
        c = expr[i]

        # Ignorar espacios
        if c.isspace():
            i += 1
            continue

        # Número (entero o flotante)
        if c.isdigit() or (c == '.' and i+1 < len(expr) and expr[i+1].isdigit()):
            m = re.match(r'\d+(\.\d+)?', expr[i:])
            val = m.group(0)
            tokens.append(Token(TT_NUM, val))
            prev_type = TT_NUM
            i += len(val)
            continue

        # Identificador (variable)
        if c.isalpha() or c == '_':
            m = re.match(r'[A-Za-z_]\w*', expr[i:])
            ident = m.group(0)
            tokens.append(Token(TT_ID, ident))
            prev_type = TT_ID
            i += len(ident)
            continue

        # Paréntesis
        if c == '(':
            tokens.append(Token(TT_LP, c))
            prev_type = TT_LP
            i += 1
            continue
        if c == ')':
            tokens.append(Token(TT_RP, c))
            prev_type = TT_RP
            i += 1
            continue

        # Operadores
        if c in "+-*/^":
            # Detección de menos unario: '-' al inicio, tras otro operador o tras '('
            if c == '-' and (prev_type in (None, TT_OP, TT_LP)):
                tokens.append(Token(TT_OP, "UMINUS"))
            else:
                tokens.append(Token(TT_OP, c))
            prev_type = TT_OP
            i += 1
            continue

        # Si llegamos aquí, el carácter no es válido
        raise ValueError(f"Carácter no reconocido: '{c}' en posición {i}")

    return tokens

def to_postfix(tokens):
    """- Usa una pila para operadores y paréntesis.
    - Respeta precedencias y asociatividades definidas en OP_INFO."""
    output = []
    stack = []
    for t in tokens:
        if t.type in (TT_NUM, TT_ID):
            # Operandos pasan directo a la salida
            output.append(t)
        elif t.type == TT_OP:
            # Mientras el topo de la pila sea un operador con mayor/igual precedencia,
            # lo pasamos a la salida.
            o1 = t.value
            while stack:
                top = stack[-1]
                if top.type != TT_OP:
                    break
                o2 = top.value
                if ((OP_INFO[o1]["assoc"] == "left" and OP_INFO[o1]["prec"] <= OP_INFO[o2]["prec"]) or
                    (OP_INFO[o1]["assoc"] == "right" and OP_INFO[o1]["prec"] <  OP_INFO[o2]["prec"])):
                    output.append(stack.pop())
                else:
                    break
            stack.append(t)
        elif t.type == TT_LP:
            stack.append(t)
        elif t.type == TT_RP:
            # Vaciar hasta encontrar '('
            while stack and stack[-1].type != TT_LP:
                output.append(stack.pop())
            if not stack:
                raise ValueError("Paréntesis desbalanceados")
            stack.pop()  # quitar '('
    # Vaciar pila restante
    while stack:
        if stack[-1].type in (TT_LP, TT_RP):
            raise ValueError("Paréntesis desbalanceados al final")
        output.append(stack.pop())
    return output

# ---- Nodo de AST ----
class Node:
    """Representa un nodo del Árbol de Sintaxis Abstracta (AST).
    - Hojas: op = 'NUM' o 'ID', value = texto del número/identificador.
    - Unario: op = 'UMINUS', left = hijo.
    - Binario: op en {+, -, *, /, ^}, left/right = hijos."""
    def __init__(self, op=None, left=None, right=None, value=None):
        self.op = op
        self.left = left
        self.right = right
        self.value = value

    @property
    def is_leaf(self):
        return self.op in ("NUM", "ID")

def postfix_to_ast(postfix):
    """Construye un AST a partir de una lista de tokens en postfija.
    - Para operando: apila nodo hoja.
    - Para operador unario: desapila 1 y crea nodo.
    - Para operador binario: desapila 2 (a,b), crea nodo (a op b) y apila."""
    st = []
    for t in postfix:
        if t.type in (TT_NUM, TT_ID):
            op = "NUM" if t.type == TT_NUM else "ID"
            st.append(Node(op=op, value=t.value))
        elif t.type == TT_OP:
            info = OP_INFO[t.value]
            if info["arity"] == 1:
                a = st.pop()
                st.append(Node(op=t.value, left=a))
            else:
                b = st.pop()
                a = st.pop()
                st.append(Node(op=t.value, left=a, right=b))
        else:
            raise ValueError("Token inesperado en postfija")
    if len(st) != 1:
        raise ValueError("Expresión inválida (AST no único)")
    return st[0]

#Utilidades de impresión de notaciones
def op_sym(op):
    """Traduce 'UMINUS' a '-' para mostrar; deja el resto igual."""
    return "-" if op == "UMINUS" else op

def inorder(n: Node):
    """Devuelve la notación infija con paréntesis para desambiguar."""
    if n.is_leaf:
        return n.value
    if n.op == "UMINUS":
        return f"(-{wrap_if_needed(n.left)})"
    return f"({inorder(n.left)} {op_sym(n.op)} {inorder(n.right)})"

def preorder(n: Node):
    """Devuelve la notación prefija (polaca)."""
    if n.is_leaf:
        return n.value
    if n.op == "UMINUS":
        return f"- {preorder(n.left)}"
    return f"{op_sym(n.op)} {preorder(n.left)} {preorder(n.right)}"

def postorder(n: Node):
    """Devuelve la notación posfija (RPN)."""
    if n.is_leaf:
        return n.value
    if n.op == "UMINUS":
        return f"{postorder(n.left)} -"
    return f"{postorder(n.left)} {postorder(n.right)} {op_sym(n.op)}"

def wrap_if_needed(n: Node):
    """Para NEG: si no es hoja, imprime con paréntesis para legibilidad."""
    if n.is_leaf:
        return n.value
    return inorder(n)

def polish_prefix(n: Node):
    """Alias para notación polaca: es el preorder."""
    return preorder(n)

# Generación de IR (Código P, Triplos, Cuádruplos)
def gen_p_code(n: Node, code=None):
    """Genera Código P (máquina de pila).
    Convención:
      - LIT c  : empuja literal numérico c
      - LOD x  : carga variable x
      - NEG    : niega tope de pila
      - ADD/SUB/MUL/DIV/POW : opera binario sobre la pila"""
    if code is None:
        code = []
    if n.is_leaf:
        if n.op == "NUM":
            code.append(("LIT", n.value))
        else:
            code.append(("LOD", n.value))
        return code
    if n.op == "UMINUS":
        gen_p_code(n.left, code)
        code.append(("NEG",))
        return code
    # Operador binario: evalúa left, right y aplica la op
    gen_p_code(n.left, code)
    gen_p_code(n.right, code)
    opmap = {"+": "ADD", "-": "SUB", "*": "MUL", "/": "DIV", "^": "POW"}
    code.append((opmap[n.op],))
    return code

def gen_triples_quads(n: Node):
    """Genera:
      - Triplos: lista de (op, arg1, arg2) con índice como resultado implícito.
      - Cuádruplos: lista de (op, arg1, arg2, res) con temporales T0, T1, ...
    Estrategia:
      - Recorrido postorden del AST; cada operación emite una instrucción y devuelve el temporal donde quedó el resultado."""
    triples = []
    quads   = []
    temp_id = [0]

    def new_temp():
        t = f"T{temp_id[0]}"
        temp_id[0] += 1
        return t

    def emit(op, a1, a2):
        idx = len(triples)
        triples.append((op, a1, a2))
        res = new_temp()
        quads.append((op, a1, a2, res))
        return idx, res

    def gen(n: Node):
        if n.is_leaf:
            return n.value, None  # si es hoja, su "resultado" es el propio valor
        if n.op == "UMINUS":
            v, _ = gen(n.left)
            idx, res = emit("NEG", v, "-")  # usamos "-" para denotar vacío en triplos binarios
            return res, idx
        # binario
        a1, _ = gen(n.left)
        a2, _ = gen(n.right)
        idx, res = emit(n.op, a1, a2)
        return res, idx

    final_res, _ = gen(n)
    return triples, quads, final_res

# Impresores de IR
def print_p_code(code):
    """Devuelve el Código P como líneas de texto legibles."""
    out = []
    for instr in code:
        if instr[0] in ("LIT", "LOD"):
            out.append(f"{instr[0]} {instr[1]}")
        elif instr[0] == "NEG":
            out.append("NEG")
        else:
            out.append(instr[0])
    return "\n".join(out)

def print_triples(triples):
    """Imprime triplos con su índice a la izquierda."""
    return "\n".join(f"[{i}] ({op}, {a1}, {a2})" for i,(op,a1,a2) in enumerate(triples))

def print_quads(quads):
    """Imprime cuádruplos como (op, arg1, arg2, res)."""
    return "\n".join(f"({op}, {a1}, {a2}, {res})" for (op,a1,a2,res) in quads)

# Interfaz
class App(tk.Tk):
    #Ventana principal
    def __init__(self):
        super().__init__()
        self.title(APP_TITLE)
        self.geometry("1024x680")
        self.minsize(900, 560)
        self._init_style()
        self._build_ui()

    def _init_style(self):
        self.style = ttk.Style(self)
        try:
            self.style.theme_use("clam")  # tema estable
        except tk.TclError:
            pass
        bg     = "#EAF4FF"  # fondo app
        fg     = "#0A0A0A"  # texto
        acc    = "#6FA8FF"  # acento en botones 
        card   = "#D6E9FF"  # paneles
        entrybg= "#E3F0FF"  # fondo de Entry y campos de texto

        # Estilos ttk
        self.configure(bg=bg)
        self.style.configure(".", background=bg, foreground=fg, fieldbackground=entrybg)
        self.style.configure("TFrame", background=bg)
        self.style.configure("Card.TFrame", background=card, relief="flat")
        self.style.configure("TLabel", background=bg, foreground=fg, font=("Segoe UI", 10))
        self.style.configure("H1.TLabel", font=("Segoe UI Semibold", 16))
        self.style.configure("H2.TLabel", font=("Segoe UI Semibold", 12))
        self.style.configure("TButton", padding=8)
        self.style.map("TButton", foreground=[("active", fg)], background=[("active", acc)])

        # Guardamos colores para reusar con widgets tk
        self.colors = {"bg": bg, "fg": fg, "acc": acc, "card": card, "entrybg": entrybg}

    def _build_ui(self):
        #Construye todos los controles de la interfaz.
        
        # Header
        header = ttk.Frame(self, style="TFrame")
        header.pack(side="top", fill="x", padx=16, pady=(14, 8))
        ttk.Label(header, text="Conversor de Expresiones y Código Intermedio", style="H1.TLabel").pack(side="left")
        ttk.Label(header, text=f"v{VERSION}", padding=(8,0)).pack(side="left")

        # Input card
        card = ttk.Frame(self, style="Card.TFrame")
        card.pack(side="top", fill="x", padx=16, pady=8)

        top_row = ttk.Frame(card, style="Card.TFrame")
        top_row.pack(fill="x", padx=12, pady=(12, 6))

        ttk.Label(top_row, text="Expresión: ", style="H2.TLabel").pack(side="left", padx=(0,8))
        self.expr_var = tk.StringVar(value="a + b * c - (d / e ^ f) * g")
        self.expr_entry = ttk.Entry(top_row, textvariable=self.expr_var, width=70)
        self.expr_entry.pack(side="left", padx=(0,8), ipady=3)

        # Lista de ejemplos prácticos
        self.example = ttk.Combobox(top_row, state="readonly", width=40,
            values=[
                "a+b*c-(d/e^f)*g",
                "-a*(b+3)",
                "x^2 + 3*x + 5",
                "(m+n)*(p-q)/r",
                "3.5*x-(y^-z)",
                "u/(-v)+w"
            ])
        self.example.set("Ejemplos…")
        self.example.pack(side="left", padx=(0,8))
        self.example.bind("<<ComboboxSelected>>", self._fill_example)

        # Botones de acción
        btns = ttk.Frame(card, style="Card.TFrame")
        btns.pack(fill="x", padx=12, pady=(0, 12))
        ttk.Button(btns, text="Convertir", command=self.convert).pack(side="left")
        ttk.Button(btns, text="Limpiar", command=self.clear_all).pack(side="left", padx=6)

        # Notebook principal
        self.nb = ttk.Notebook(self)
        self.nb.pack(expand=True, fill="both", padx=16, pady=8)

        # Tab: Notaciones
        tab_not = ttk.Frame(self.nb, style="TFrame")
        self.nb.add(tab_not, text="Notaciones")
        self.infija_txt  = self._make_readonly_box(tab_not, "Infija")
        self.prefija_txt = self._make_readonly_box(tab_not, "Prefija")
        self.posfija_txt = self._make_readonly_box(tab_not, "Posfija")

        # Tab: Código Intermedio (sub-notebook)
        tab_ir = ttk.Frame(self.nb, style="TFrame")
        self.nb.add(tab_ir, text="Código Intermedio")

        self.nb_ir = ttk.Notebook(tab_ir)
        self.nb_ir.pack(expand=True, fill="both", padx=6, pady=6)

        self.polaca_txt = self._make_scrolled_tab(self.nb_ir, "Notación Polaca")
        self.pcode_txt  = self._make_scrolled_tab(self.nb_ir, "Código P")
        self.tri_txt    = self._make_scrolled_tab(self.nb_ir, "Triplos")
        self.quad_txt   = self._make_scrolled_tab(self.nb_ir, "Cuádruplos")

        # Barra de estado (mensajes al usuario)
        self.status = tk.StringVar(value="Listo. Ingresa una expresión y pulsa Convertir.")
        status_bar = ttk.Label(self, textvariable=self.status, anchor="w")
        status_bar.pack(side="bottom", fill="x", padx=16, pady=(0,8))

    # Helpers de UI
    def _make_readonly_box(self, parent, title):
        #Crea un bloque con etiqueta + Text de solo lectura (para notaciones).
        frame = ttk.Frame(parent)
        frame.pack(fill="x", padx=6, pady=6)
        ttk.Label(frame, text=title, style="H2.TLabel").pack(anchor="w", pady=(0,4))
        txt = tk.Text(frame, height=2, wrap="word", bd=0, relief="flat",
                      bg=self.colors["card"], fg=self.colors["fg"], insertbackground=self.colors["fg"])
        txt.pack(fill="x")
        txt.configure(state="disabled")
        return txt

    def _make_scrolled_tab(self, nb, title):
        #Crea una pestaña con ScrolledText para mostrar secciones largas (IR).
        tab = ttk.Frame(nb)
        nb.add(tab, text=title)
        txt = ScrolledText(tab, wrap="none", bd=0, relief="flat",
                           bg=self.colors["card"], fg=self.colors["fg"], insertbackground=self.colors["fg"])
        txt.pack(expand=True, fill="both")
        txt.configure(state="disabled")
        return txt

    def _fill_example(self, _evt=None):
        
        #Rellena el Entry con el ejemplo seleccionado
        val = self.example.get()
        if val and "Ejemplos" not in val:
            self.expr_var.set(val)

    def _set_text(self, widget: tk.Text, content: str):
        widget.configure(state="normal")
        widget.delete("1.0", "end")
        widget.insert("1.0", content.strip() + ("\n" if content and not content.endswith("\n") else ""))
        widget.configure(state="disabled")

    #Acciones principales
    def convert(self):
        """Lee la expresión, tokeniza, convierte a postfija, arma el AST y
        muestra: infija, prefija, posfija, polaca, Código P, triplos y cuádruplos."""
        expr = self.expr_var.get().strip()
        if not expr:
            messagebox.showinfo("Aviso", "Escribe una expresión.")
            return
        try:
            # 1) Tokenización
            toks = tokenize(expr)
            # 2) Postfija vía Shunting-yard
            post = to_postfix(toks)
            # 3) AST desde postfija
            ast = postfix_to_ast(post)

            # Notaciones
            infija  = inorder(ast)
            prefija = preorder(ast)
            posfija = " ".join(tok.value if tok.type != TT_OP else (op_sym(tok.value)) for tok in post)

            #IR
            polaca = polish_prefix(ast)
            pcode  = gen_p_code(ast)
            triples, quads, final_res = gen_triples_quads(ast)

            # Mostrar en UI
            self._set_text(self.infija_txt,  infija)
            self._set_text(self.prefija_txt, prefija)
            self._set_text(self.posfija_txt, posfija)

            self._set_text(self.polaca_txt, polaca)
            self._set_text(self.pcode_txt,  print_p_code(pcode))
            self._set_text(self.tri_txt,    print_triples(triples))
            self._set_text(self.quad_txt,   print_quads(quads) + f"\n\nResultado final en: {final_res}")

            self.status.set("Conversión correcta.")

        except Exception as e:
            # Cualquier error de análisis cae aquí
            self.status.set("Error de análisis.")
            messagebox.showerror("Error", f"Ocurrió un error:\n\n{e}")

    def clear_all(self):
        #Limpia la expresión y todas las áreas de salida.
        self.expr_var.set("")
        for w in [self.infija_txt, self.prefija_txt, self.posfija_txt, self.polaca_txt, self.pcode_txt, self.tri_txt, self.quad_txt]:
            self._set_text(w, "")
        self.status.set("Campos limpios. ¿Otra vuelta?")

# Punto de entrada
def main():
    app = App()
    app.mainloop()

if __name__ == "__main__":
    if sys.version_info < (3,8):
        print("Se recomienda Python 3.8+")
    main()