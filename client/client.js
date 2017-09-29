const app = Elm.Main.fullscreen()

const socket = io.connect('/')


// OUTGOING

app.ports.createRoom.subscribe(data => {
  socket.emit('create-room', data)
})

app.ports.joinRoom.subscribe(data => {
  socket.emit('join-room', data)
})

app.ports.sendTriplet.subscribe(data => {
  socket.emit('triplet', data)
})

app.ports.requestToppingList.subscribe(data => {
  socket.emit('request-topping-list', data)
})

app.ports.sendToppingListOrError.subscribe(data => {
  socket.emit('topping-list', data)
})

app.ports.sendKickGuest.subscribe(data => {
  socket.emit('kick-guest', data)
})


// INCOMING

socket.on('create-room-response', data => {
  app.ports.createRoomResponse.send(data)
})

socket.on('join-room-response', data => {
  app.ports.joinRoomResponse.send(data)
})

socket.on('triplet', data => {
  app.ports.receiveTriplet.send(data)
})

socket.on('request-topping-list', data => {
  app.ports.receiveToppingListRequest.send(data)
})

socket.on('topping-list', data => {
  app.ports.receiveToppingList.send(data)
})

socket.on('guest-left', data => {
  app.ports.receiveGuestLeft.send(data)
})

socket.on('host-left', data => {
  app.ports.receiveHostLeft.send(data)
})

socket.on('kick-guest', data => {
  app.ports.receiveKickGuest.send(data)
})