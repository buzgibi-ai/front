export const _create = 
  function create(url) {
    return function (protocols) {
        let ws = new WebSocket(url, protocols);
        ws.binaryType = "blob";
        return ws;
      };
 };

export const _readState = function(ws) {
    return function () { return ws.readyState; };
}