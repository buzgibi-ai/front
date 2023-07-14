import * as e from '../Buzgibi.Api.Foreign.BuzgibiBack/BuzgibiBack/src/index';

export const mkUserApi = function(api) {
    return () => {
        return new e.UserApi(api);
    }
}

export const _makeEnquiry = function(withError, enquiry, api) {
    return function(onError, onOk) {
        api.userEnquiryPost(enquiry).then(onOk).catch(resp => {
            return withError(resp, onError)
        })
    };
}