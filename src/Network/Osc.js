"use strict";

export const

export const _oscNewWebSocketPort = url => () =>
  new osc.WebSocketPort({ "url": url });

export const _oscPortOpen = port => () => port.open();
export const _oscPortSend = port => msg => () => port.send(msg);

export const _oscPortOnMessage = port => call => () => {
    port.on("message", function (msg) {
      call(msg)();
    });
  }
