exports.differentObject = function(obj1) {
    return function(obj2) {
        return (obj1 !== obj2);
    }
}


exports.handlerWithDefault = function (key, action) {
    return [key, function (input, parentAction) {
        return function (ev) {
            input(parentAction(action(ev)))();
        };
    }];
};
