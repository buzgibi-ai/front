import * as e from '../Buzgibi.Api.Foreign.BuzgibiBack/BuzgibiBack/src/index';

export const mkAuthApi = function(api) {
    return () => {
        return new e.AuthApi(api);
    }
}

export const _register = 
function(withError, cred, api) {
    return function(onError, onOk) {
        api.authRegisterPost(cred).then(onOk).catch(resp => {
            return withError(resp, onError)
        })
    };
}

export const _login = 
function(withError, authType, cred, api) {
    return function(onError, onOk) {
        api.authLoginAuthTypePost(authType, cred).then(onOk).catch(resp => {
            return withError(resp, onError)
        })
    };
}