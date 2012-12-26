require 'bootstrap-sass'

http_path = '/static/'
sass_path = 'styles'
images_path = 'images'
css_dir = 'public/css'
generated_images_dir = 'public/img'
http_generated_images_path = '/static/img'

#http_generated_images_path = '/static/images'
#generated_images_dir = 'images'
#http_fonts_path = '/static/fonts'

# PNG optimization
#on_sprite_saved do |filename|
#  `pngcrush -rem alla -reduce -brute #{filename} #{filename}.sup`
#  `mv #{filename}.sup #{filename}`
#end

# CSSO
# on_stylesheet_saved do |filename|
#   `csso -i #{filename} -o #{filename}`
# end