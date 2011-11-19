var ThouMakethCommentsShine = function(num, box) {
    var box = $(box), form = $('form', box), sbmt = $('button[type=submit]', box);

    var state = true;
    var toggle = function(){
        sbmt.attr('disabled', state);
        $('textarea', box).attr('disabled', state).val('');
        state = !state;
    }
    
    var handleFormSubmission = function(data){
        if (!data['status']) {
            // make errors shine
        } else {
            $('dl', box).append(data['html']);
            toggle();
        }
    }
    
    var submitForm = function(){
        $.post(form.attr('action'), form.serialize(), handleFormSubmission);
        toggle();
        return false;
    }
    
    form.submit(submitForm);
}
$(document).ready(function(){
  $('div.comments').each(ThouMakethCommentsShine);
});
