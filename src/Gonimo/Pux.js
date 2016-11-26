

exports.handlerWithDefault = function (key, action) {
    return [key, function (input, parentAction) {
        return function (ev) {
            input(parentAction(action(ev)))();
        };
    }];
};
