
exports.copyTextFromId = function(id_) {
    return function() {
            try
            {
                var textBox = document.querySelector("#" + id_);
                textBox.select();
                return document.execCommand('copy');
            }
            catch (e) {
                return false;
            }
    };
}
