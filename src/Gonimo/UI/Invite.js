
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
};

exports.handlerWithCopy = function (id_, key, action) {
    return [key, function (input, parentAction) {
        return function (ev) {
            exports.copyTextFromId(id_)();
            if ((key === 'onSubmit')
                || (key === 'onClick' && ev.currentTarget.nodeName.toLowerCase() === 'a')) {
                ev.preventDefault();
            }
            input(parentAction(action(ev)))();
        };
    }];
};
