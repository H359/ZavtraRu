var ThouMakethCommentsShine = function(num, box) {
    var box = $(box), form = $('form', box), sbmt = $('button[type=submit]', box);
    
    var handleFormSubmission = function(data){
        if (!data['status']) {
            // make errors shine
        } else {
            $('dl', box).append(data['html']);
            sbmt.attr('disabled', false);
        }
    }
    
    var submitForm = function(){
        $.post(form.attr('action'), form.serialize(), handleFormSubmission);
        sbmt.attr('disabled', true);
        return false;
    }
    
    form.submit(submitForm);
}

$(document).ready(function(){
    $('div.comments').each(ThouMakethCommentsShine);
});
