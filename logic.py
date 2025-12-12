import os
from PIL import Image

class ImageManager:
    def __init__(self):
        # We'll store PIL Images in a list [Img1, Img2, Result]
        # None indicates empty slot
        self.images = [None, None, None]
        self.width = 0
        self.height = 0

    def load_image(self, index, filepath):
        if not os.path.exists(filepath):
            return
        
        try:
            # Load and convert to RGB
            img = Image.open(filepath).convert('RGB')
            
            # If it's the first image (index 0) or we don't have dims yet, set them
            if index == 0 or (self.width == 0 and self.height == 0):
                self.width, self.height = img.size
            else:
                # Resize to match our current working dimensions
                if img.size != (self.width, self.height):
                    img = img.resize((self.width, self.height))
            
            self.images[index] = img
            
        except Exception as e:
            print(f"Error loading image: {e}")

    def get_image(self, index):
        if 0 <= index < 3:
            return self.images[index]
        return None

    def validate_images(self):
        # Checks if Img1 and Img2 are loaded and valid
        return (self.images[0] is not None and 
                self.images[1] is not None and 
                self.width > 0 and self.height > 0)

    # --- Operations ---

    def sum_images_avg(self):
        if not self.validate_images(): return
        
        res = Image.new('RGB', (self.width, self.height))
        pixels_res = res.load()
        pixels1 = self.images[0].load()
        pixels2 = self.images[1].load()
        
        for x in range(self.width):
            for y in range(self.height):
                r1, g1, b1 = pixels1[x, y]
                r2, g2, b2 = pixels2[x, y]
                
                pixels_res[x, y] = (
                    (r1 + r2) // 2,
                    (g1 + g2) // 2,
                    (b1 + b2) // 2
                )
        self.images[2] = res

    def sum_images_clip(self):
        if not self.validate_images(): return
        
        res = Image.new('RGB', (self.width, self.height))
        pixels_res = res.load()
        pixels1 = self.images[0].load()
        pixels2 = self.images[1].load()
        
        for x in range(self.width):
            for y in range(self.height):
                r1, g1, b1 = pixels1[x, y]
                r2, g2, b2 = pixels2[x, y]
                
                pixels_res[x, y] = (
                    min(255, r1 + r2),
                    min(255, g1 + g2),
                    min(255, b1 + b2)
                )
        self.images[2] = res

    def sub_images_zero(self):
        if not self.validate_images(): return
        
        res = Image.new('RGB', (self.width, self.height))
        pixels_res = res.load()
        pixels1 = self.images[0].load()
        pixels2 = self.images[1].load()
        
        for x in range(self.width):
            for y in range(self.height):
                r1, g1, b1 = pixels1[x, y]
                r2, g2, b2 = pixels2[x, y]
                
                pixels_res[x, y] = (
                    max(0, r1 - r2),
                    max(0, g1 - g2),
                    max(0, b1 - b2)
                )
        self.images[2] = res

    def sub_images_abs(self):
        if not self.validate_images(): return
        
        res = Image.new('RGB', (self.width, self.height))
        pixels_res = res.load()
        pixels1 = self.images[0].load()
        pixels2 = self.images[1].load()
        
        for x in range(self.width):
            for y in range(self.height):
                r1, g1, b1 = pixels1[x, y]
                r2, g2, b2 = pixels2[x, y]
                
                pixels_res[x, y] = (
                    abs(r1 - r2),
                    abs(g1 - g2),
                    abs(b1 - b2)
                )
        self.images[2] = res

    def sub_images_avg_shift(self):
        if not self.validate_images(): return
        
        res = Image.new('RGB', (self.width, self.height))
        pixels_res = res.load()
        pixels1 = self.images[0].load()
        pixels2 = self.images[1].load()
        
        for x in range(self.width):
            for y in range(self.height):
                r1, g1, b1 = pixels1[x, y]
                r2, g2, b2 = pixels2[x, y]
                
                pixels_res[x, y] = (
                    127 + (r1 - r2) // 2,
                    127 + (g1 - g2) // 2,
                    127 + (b1 - b2) // 2
                )
        self.images[2] = res

    # --- Reflections ---

    def reflect_horizontal(self, index):
        if self.images[index] is None: return
        
        img = self.images[index]
        pixels = img.load()
        w, h = img.size
        
        # In-place horizontal reflection
        for x in range(w // 2):
            for y in range(h):
                left_pixel = pixels[x, y]
                right_pixel = pixels[w - 1 - x, y]
                
                pixels[x, y] = right_pixel
                pixels[w - 1 - x, y] = left_pixel

    def reflect_vertical(self, index):
        if self.images[index] is None: return
        
        img = self.images[index]
        pixels = img.load()
        w, h = img.size
        
        # In-place vertical reflection
        for x in range(w):
            for y in range(h // 2):
                top_pixel = pixels[x, y]
                bottom_pixel = pixels[x, h - 1 - y]
                
                pixels[x, y] = bottom_pixel
                pixels[x, h - 1 - y] = top_pixel

    def reflect_double(self, index):
        if self.images[index] is None: return
        self.reflect_horizontal(index)
        self.reflect_vertical(index)
