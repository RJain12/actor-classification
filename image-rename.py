import os

def rename_images(directory):
    image_files = []
    
    # Get a list of all image files in the directory
    for filename in os.listdir(directory):
        if filename.endswith('.jpg') or filename.endswith('.png'):
            image_files.append(filename)
    
    # Sort the image files in ascending order
    image_files.sort()
    
    # Rename the image files
    for i, filename in enumerate(image_files, start=1):
        extension = os.path.splitext(filename)[1]
        new_filename = f"1_{i}{extension}"
        new_filepath = os.path.join(directory, new_filename)
        old_filepath = os.path.join(directory, filename)
        os.rename(old_filepath, new_filepath)
        print(f"Renamed '{filename}' to '{new_filename}'.")

# Specify the directory where the images are located
image_directory = 'images'

# Call the function to rename the images
rename_images(image_directory)
