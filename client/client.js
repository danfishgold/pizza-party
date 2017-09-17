const app = Elm.Main.fullscreen()

const socket = io.connect('/')


// OUTGOING

app.ports.connectAsHost.subscribe(data => {
  socket.emit('connect-as-host', data)
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

socket.on('connect-as-host-response', data => {
  app.ports.connectAsHostResponse.send(data)
})

socket.on('triplet', data => {
  app.ports.receiveTriplet.send(data)
})

socket.on('request-topping-list', data => {
  console.log('hey')
  app.ports.receiveToppingListRequest.send(data)
})

socket.on('topping-list', data => {
  app.ports.receiveToppingList.send(data)
})