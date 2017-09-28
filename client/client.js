const app = Elm.Main.fullscreen()

const socket = io.connect('http://localhost:5000')


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