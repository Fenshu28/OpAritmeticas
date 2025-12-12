import tkinter as tk
from tkinter import filedialog, messagebox, ttk
from PIL import Image, ImageTk, ImageChops, ImageMath
import os
import sys

class ImageApp:
    def __init__(self, root):
        self.root = root
        self.root.title("Operaciones Aritméticas de Imágenes")
        self.root.geometry("1100x650")
        
        # State
        self.img1_original = None
        self.img2_original = None
        self.img3_result = None
        self.base_width = 300
        self.base_height = 250
        
        # --- GUI Layout ---
        # Ahora podrias hacer en lazarus primero que pueda cambiar de lugar las imagenes, es simplemente hacer cambio entre matrices de la A a la B, o de la imagen B a la A o de la C a la A, C la B. Lo que no puedo hacer es cambiar la A o B a la C.
        # C es el resultado.
        
        # Header / Reflections Group
        self.frame_top = tk.LabelFrame(root, text="Operaciones de reflexión", padx=10, pady=10)
        self.frame_top.pack(side=tk.TOP, fill=tk.X, padx=10, pady=5)
        
        tk.Label(self.frame_top, text="Elige la imagen:").pack(side=tk.LEFT, padx=5)
        self.cb_reflexion = ttk.Combobox(self.frame_top, values=["img1", "img2", "result"], state="readonly", width=10)
        self.cb_reflexion.current(0)
        self.cb_reflexion.pack(side=tk.LEFT, padx=5)
        
        tk.Label(self.frame_top, text="¿Qué reflexión?").pack(side=tk.LEFT, padx=15)
        tk.Button(self.frame_top, text="Vertical", command=self.on_vertical_click).pack(side=tk.LEFT, padx=5)
        tk.Button(self.frame_top, text="Horizontal", command=self.on_horizontal_click).pack(side=tk.LEFT, padx=5)
        tk.Button(self.frame_top, text="Doble", command=self.on_doble_click).pack(side=tk.LEFT, padx=5)
        
        # Images Panel
        self.frame_images = tk.Frame(root, bg="#FFFFE0") # Light yellow background like in Pascal
        self.frame_images.pack(expand=True, fill=tk.BOTH, padx=10, pady=5)
        
        # Image 1 display
        self.lbl_img1 = tk.Label(self.frame_images, bg="#CCCCCC", text="IMG 1", relief=tk.SUNKEN)
        self.lbl_img1.place(x=20, y=20, width=300, height=250)
        
        # Plus sign
        self.lbl_oper = tk.Label(self.frame_images, text="+", bg="#FFFFE0", font=("Arial", 24))
        self.lbl_oper.place(x=340, y=120)
        
        # Image 2 display
        self.lbl_img2 = tk.Label(self.frame_images, bg="#CCCCCC", text="IMG 2", relief=tk.SUNKEN)
        self.lbl_img2.place(x=380, y=20, width=300, height=250)
        
        # Equal sign
        self.lbl_eq = tk.Label(self.frame_images, text="=", bg="#FFFFE0", font=("Arial", 24))
        self.lbl_eq.place(x=700, y=120)
        
        # Result Image display
        self.lbl_img3 = tk.Label(self.frame_images, bg="#CCCCCC", text="RESULTADO", relief=tk.SUNKEN)
        self.lbl_img3.place(x=740, y=20, width=300, height=250)
        
        # Bottom Controls
        self.frame_bottom = tk.Frame(root, pady=10)
        self.frame_bottom.pack(side=tk.BOTTOM, fill=tk.X, padx=10)
        
        tk.Button(self.frame_bottom, text="Cargar img1", command=self.load_img1, width=15).pack(side=tk.LEFT, padx=20)
        
        self.cb_opera = ttk.Combobox(self.frame_bottom, values=[
            "Suma1 (Promedio)", 
            "Suma2 (Saturada)", 
            "Resta1 (A cero)", 
            "Resta2 (Absoluta)", 
            "Resta3 (Ajuste Media)"
        ], state="readonly", width=25)
        self.cb_opera.current(0)
        self.cb_opera.pack(side=tk.LEFT, padx=10)
        self.cb_opera.bind("<<ComboboxSelected>>", self.on_operation_change)
        
        tk.Button(self.frame_bottom, text="Cargar img2", command=self.load_img2, width=15).pack(side=tk.LEFT, padx=20)
        
        tk.Button(self.frame_bottom, text="Operar", command=self.on_operar_click, width=15, bg="#DDDDDD").pack(side=tk.LEFT, padx=20)
        
        tk.Button(self.frame_bottom, text="Descargar", command=self.download_result, width=15).pack(side=tk.RIGHT, padx=20)

    # --- Helpers ---
    
    def set_loading(self, loading):
        if loading:
            self.root.config(cursor="watch")
            self.root.update()
        else:
            self.root.config(cursor="")

    def resize_for_display(self, pil_img):
        # Resize to fit 300x250 keeping aspect ratio or stretch?
        # Pascal code uses TImage.Stretch = True, so we resize to exactly 300x250
        return pil_img.resize((300, 250), Image.Resampling.LANCZOS)

    def update_display(self, index):
        target_lbl = None
        source_img = None
        
        if index == 0:
            target_lbl = self.lbl_img1
            source_img = self.img1_original
        elif index == 1:
            target_lbl = self.lbl_img2
            source_img = self.img2_original
        elif index == 2:
            target_lbl = self.lbl_img3
            source_img = self.img3_result
            
        if source_img:
            thumb = self.resize_for_display(source_img)
            tk_thumb = ImageTk.PhotoImage(thumb)
            target_lbl.config(image=tk_thumb, text="")
            target_lbl.image = tk_thumb # Keep reference
        else:
            target_lbl.config(image="", text="Sin Imagen")

    def ensure_rgb(self, img):
        if img.mode != 'RGB':
            return img.convert('RGB')
        return img
    
    def match_size(self, source, element_to_match):
        # Resize source to match 'element_to_match' size
        if source.size != element_to_match.size:
            return source.resize(element_to_match.size, Image.Resampling.LANCZOS)
        return source

    # --- Loaders ---

    def load_img1(self):
        path = filedialog.askopenfilename(filetypes=[("Imágenes", "*.jpg *.jpeg *.png *.bmp")])
        if path:
            self.set_loading(True)
            try:
                self.img1_original = Image.open(path).convert("RGB")
                self.update_display(0)
            finally:
                self.set_loading(False)

    def load_img2(self):
        path = filedialog.askopenfilename(filetypes=[("Imágenes", "*.jpg *.jpeg *.png *.bmp")])
        if path:
            self.set_loading(True)
            try:
                self.img2_original = Image.open(path).convert("RGB")
                self.update_display(1)
            finally:
                self.set_loading(False)

    def download_result(self):
        if not self.img3_result:
            messagebox.showwarning("Aviso", "No hay resultado para descargar.")
            return
            
        path = filedialog.asksaveasfilename(defaultextension=".png", 
                                            filetypes=[("PNG", "*.png"), ("JPG", "*.jpg")])
        if path:
            try:
                self.img3_result.save(path)
                messagebox.showinfo("Éxito", "Imagen guardada exitosamente.")
            except Exception as e:
                messagebox.showerror("Error", str(e))

    def on_operation_change(self, event):
        op = self.cb_opera.get()
        if "Suma" in op:
            self.lbl_oper.config(text="+")
        else:
            self.lbl_oper.config(text="-")

    # --- Operations logic ---

    def on_operar_click(self):
        if not self.img1_original or not self.img2_original:
            messagebox.showerror("Error", "Debes cargar ambas imágenes (img1 e img2).")
            return

        self.set_loading(True)
        try:
            # Normalize sizes: Resize Image 2 to match Image 1
            i1 = self.img1_original
            i2 = self.match_size(self.img2_original, i1)
            
            op = self.cb_opera.get()
            
            if "Suma1" in op:
                # Average: (A + B) / 2
                # PIL.Image.blend(im1, im2, alpha=0.5) does exactly this
                self.img3_result = Image.blend(i1, i2, 0.5)
                
            elif "Suma2" in op:
                # Clamped Sum: min(A + B, 255)
                # We can specificy this per band
                bands1 = i1.split()
                bands2 = i2.split()
                result_bands = []
                for b1, b2 in zip(bands1, bands2):
                    # Using ImageMath
                    # 'convert(min(a+b, 255), "L")'
                    out = ImageMath.eval("convert(min(float(a)+float(b), 255), 'L')", a=b1, b=b2)
                    result_bands.append(out)
                self.img3_result = Image.merge("RGB", result_bands)
                
            elif "Resta1" in op:
                # Clamped Sub: max(A - B, 0)
                # ImageChops.subtract does exactly this (sets negatives to 0)
                self.img3_result = ImageChops.subtract(i1, i2, 1.0, 0)
                
            elif "Resta2" in op:
                # Absolute Diff: abs(A - B)
                # ImageChops.difference does exactly this
                self.img3_result = ImageChops.difference(i1, i2)
                
            elif "Resta3" in op:
                # Mean Shift: 127 + (A - B) / 2
                bands1 = i1.split()
                bands2 = i2.split()
                result_bands = []
                for b1, b2 in zip(bands1, bands2):
                    # 127 + (a - b) // 2
                    # Note: Using float for correct division then casting to int/L
                    out = ImageMath.eval("convert(127 + (float(a)-float(b))/2, 'L')", a=b1, b=b2)
                    result_bands.append(out)
                self.img3_result = Image.merge("RGB", result_bands)
            
            self.update_display(2)
            
        except Exception as e:
            messagebox.showerror("Error en operación", str(e))
        finally:
            self.set_loading(False)

    def get_target_image_info(self):
        selection = self.cb_reflexion.get()
        if selection == "img1":
            return 0, self.img1_original
        elif selection == "img2":
            return 1, self.img2_original
        elif selection == "result":
            return 2, self.img3_result
        return -1, None

    def update_target_image(self, index, new_img):
        if index == 0:
            self.img1_original = new_img
        elif index == 1:
            self.img2_original = new_img
        elif index == 2:
            self.img3_result = new_img
        self.update_display(index)

    def perform_reflection(self, method):
        idx, img = self.get_target_image_info()
        if not img:
            messagebox.showwarning("Aviso", f"La imagen seleccionada ({self.cb_reflexion.get()}) no está cargada.")
            return
            
        self.set_loading(True)
        try:
            # Pillow has built-in Transpose methods
            if method == "horiz":
                new_img = img.transpose(Image.FLIP_LEFT_RIGHT)
            elif method == "vert":
                new_img = img.transpose(Image.FLIP_TOP_BOTTOM)
            elif method == "doble":
                # Horizontal + Vertical is equivalent to Rotate 180
                new_img = img.transpose(Image.ROTATE_180)
            
            self.update_target_image(idx, new_img)
            
        finally:
            self.set_loading(False)

    def on_vertical_click(self):
        self.perform_reflection("vert")

    def on_horizontal_click(self):
        self.perform_reflection("horiz")

    def on_doble_click(self):
        self.perform_reflection("doble")

if __name__ == "__main__":
    try:
        # High DPI support for Windows
        from ctypes import windll
        windll.shcore.SetProcessDpiAwareness(1)
    except:
        pass

    root = tk.Tk()
    app = ImageApp(root)
    root.mainloop()
