import tkinter as tk
from tkinter import filedialog, messagebox, ttk
from PIL import ImageTk
from logic import ImageManager

class App(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("Operaciones Aritméticas")
        self.geometry("1050x550")
        
        self.manager = ImageManager()
        
        # === UI Layout ===
        
        # Top Panel for Images
        self.panel_imgs = tk.Frame(self, bg="#f0f0f0", height=300)
        self.panel_imgs.pack(fill=tk.X, padx=10, pady=10)
        
        # Canvas placeholders
        self.lbl_img1 = tk.Label(self.panel_imgs, text="Img 1", bg="white", width=40, height=15)
        self.lbl_img1.grid(row=0, column=0, padx=5)
        
        self.lbl_op = tk.Label(self.panel_imgs, text="+", font=("Arial", 20))
        self.lbl_op.grid(row=0, column=1, padx=5)
        
        self.lbl_img2 = tk.Label(self.panel_imgs, text="Img 2", bg="white", width=40, height=15)
        self.lbl_img2.grid(row=0, column=2, padx=5)
        
        self.lbl_eq = tk.Label(self.panel_imgs, text="=", font=("Arial", 20))
        self.lbl_eq.grid(row=0, column=3, padx=5)
        
        self.lbl_img3 = tk.Label(self.panel_imgs, text="Result", bg="white", width=40, height=15)
        self.lbl_img3.grid(row=0, column=4, padx=5)
        
        # Reflection Controls Group
        self.grp_reflect = tk.LabelFrame(self, text="Operaciones de reflexión", padx=10, pady=10)
        self.grp_reflect.pack(padx=10, pady=5, fill=tk.X)
        
        tk.Label(self.grp_reflect, text="Elige la imagen:").grid(row=0, column=0, padx=5)
        self.cb_reflect_target = ttk.Combobox(self.grp_reflect, values=["img1", "img2", "result"], state="readonly")
        self.cb_reflect_target.current(0)
        self.cb_reflect_target.grid(row=1, column=0, padx=5)
        
        self.btn_vert = tk.Button(self.grp_reflect, text="Vertical", command=self.on_vertical)
        self.btn_vert.grid(row=1, column=1, padx=5)
        
        self.btn_horz = tk.Button(self.grp_reflect, text="Horizontal", command=self.on_horizontal)
        self.btn_horz.grid(row=1, column=2, padx=5)
        
        self.btn_dbl = tk.Button(self.grp_reflect, text="Doble", command=self.on_double)
        self.btn_dbl.grid(row=1, column=3, padx=5)
        
        # Bottom Controls
        self.frm_controls = tk.Frame(self)
        self.frm_controls.pack(pady=10)
        
        self.btn_load1 = tk.Button(self.frm_controls, text="Cargar img1", command=lambda: self.load_img(0))
        self.btn_load1.pack(side=tk.LEFT, padx=10)
        
        self.cb_operation = ttk.Combobox(self.frm_controls, values=["Suma1 (Avg)", "Suma2 (Clip)", "Resta1 (Zero)", "Resta2 (Abs)", "Resta3 (AvgShift)"], state="readonly")
        self.cb_operation.current(0)
        self.cb_operation.pack(side=tk.LEFT, padx=10)
        self.cb_operation.bind("<<ComboboxSelected>>", self.update_op_label)
        
        self.btn_load2 = tk.Button(self.frm_controls, text="Cargar img2", command=lambda: self.load_img(1))
        self.btn_load2.pack(side=tk.LEFT, padx=10)
        
        self.btn_operate = tk.Button(self.frm_controls, text="Operar", command=self.on_operate)
        self.btn_operate.pack(side=tk.LEFT, padx=10)
        
    def update_display(self, index):
        img_pil = self.manager.get_image(index)
        if img_pil:
            # Resize for thumbnail display only
            thumb = img_pil.copy()
            thumb.thumbnail((300, 250))
            photo = ImageTk.PhotoImage(thumb)
            
            target = [self.lbl_img1, self.lbl_img2, self.lbl_img3][index]
            target.config(image=photo, width=0, height=0) # Reset w/h to let image dictate size
            target.image = photo # Keep reference

    def load_img(self, index):
        path = filedialog.askopenfilename(filetypes=[("Images", "*.png;*.jpg;*.jpeg;*.bmp")])
        if path:
            self.manager.load_image(index, path)
            self.update_display(index)

    def update_op_label(self, event):
        op = self.cb_operation.get()
        if "Suma" in op:
            self.lbl_op.config(text="+")
        else:
            self.lbl_op.config(text="-")

    def on_operate(self):
        op = self.cb_operation.get()
        if "Suma1" in op:
            self.manager.sum_images_avg()
        elif "Suma2" in op:
            self.manager.sum_images_clip()
        elif "Resta1" in op:
            self.manager.sub_images_zero()
        elif "Resta2" in op:
            self.manager.sub_images_abs()
        elif "Resta3" in op:
            self.manager.sub_images_avg_shift()
            
        self.update_display(2)

    def get_target_index(self):
        val = self.cb_reflect_target.get()
        if val == "img1": return 0
        if val == "img2": return 1
        return 2

    def on_vertical(self):
        idx = self.get_target_index()
        self.manager.reflect_vertical(idx)
        self.update_display(idx)

    def on_horizontal(self):
        idx = self.get_target_index()
        self.manager.reflect_horizontal(idx)
        self.update_display(idx)

    def on_double(self):
        idx = self.get_target_index()
        self.manager.reflect_double(idx)
        self.update_display(idx)

if __name__ == "__main__":
    app = App()
    app.mainloop()
