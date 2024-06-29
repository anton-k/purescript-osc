"use strict";

export const _oscNewWebSocketPort = url => () =>
  new osc.WebSocketPort({ "url": url });

export const _oscPortOpen = port => () => port.open();
export const _oscPortSend = port => msg => () => port.send(msg);
