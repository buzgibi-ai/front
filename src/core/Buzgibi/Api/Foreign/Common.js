import * as e from '../Buzgibi.Api.Foreign.BuzgibiBack/BuzgibiBack/src/index';

export const _mkApiClient = function(jwt, host) {
    return () => {
        let cl = new e.ApiClient(host);
        if (jwt !== undefined) {
            cl.defaultHeaders = {
                Authorization: 'Bearer ' + jwt
            };
        } else {
            cl.defaultHeaders = [];
        }
        return cl;
    }
}

export const _getDataFromObj = left => right => resp => {
    let success = resp.getSuccess();

    let errMsg = (xs) => {
        tmp = '';
        xs.forEach(e => {
            tmp += e.getMessage();
        });
        return tmp;
    }
    let err = resp.getErrors() !== undefined ? errMsg(resp.getErrors()) : 'malformed resp: ' + JSON.stringify(resp);
    return () => {
        return success !== undefined ? right(success) : left(err);
    };
}

export const _printError = (err) => {
    return err.getMessage();
}

export const withError = function(resp, onError) {
    let e = new Error();
    let mkMsg = '';
    if (resp['body']['combinator'] !== undefined) {
        mkMsg += "combinator " + resp['body']['combinator'] + " has failed with error " + resp['body']['error'];
    } else if (resp['body'] != undefined) {
        mkMsg += "server responded with " + resp['body'];
    } else {
        mkMsg += 'server responded with an unknown error';
    }
    e.message = "status: " + resp['status'] + ". error: " + mkMsg;
    onError(e);
}