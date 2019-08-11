const app = Elm.Main.init()

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

app.ports.requestBaseToppingList.subscribe(data => {
  socket.emit('request-base-topping-list', data)
})

app.ports.sendBaseToppingListOrError.subscribe(data => {
  socket.emit('base-topping-list', data)
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

socket.on('request-base-topping-list', data => {
  app.ports.receiveBaseToppingListRequest.send(data)
})

socket.on('base-topping-list', data => {
  app.ports.receiveBaseToppingList.send(data)
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
