sass_path = "sass"

print "SASS path = #{sass_path} \n"

css_path   = File.join "../static/css"
images_dir = File.join "../static/img"

print "Images dir: #{images_dir} \n"

output_style = :compressed
environment  = :production

$shouldTouch = true

on_stylesheet_saved do |filename|
    print "saved "
    print filename
    print "\n"
    if $shouldTouch
	$shouldTouch = false
	`touch ../Foundation.hs`
	print "fouched foundation"
    end
end
