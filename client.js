const app = Elm.Main.init()

const socket = io.connect('/')

// OUTGOING

app.ports.sendCreateRoom.subscribe(data => {
  socket.emit('create-room', data)
})

app.ports.sendFindRoom.subscribe(data => {
  socket.emit('find-room', data)
})

app.ports.sendJoinRoom.subscribe(data => {
  socket.emit('join-room', data)
})

app.ports.sendUpdateTriplet.subscribe(data => {
  socket.emit('triplet-update', data)
})

app.ports.sendKickGuest.subscribe(data => {
  socket.emit('kick-guest', data)
})

// INCOMING

socket.on('create-room-response', data => {
  console.log('create-room-response')
  app.ports.receiveCreateRoomResponse.send(data)
})

socket.on('find-room-response', data => {
  console.log('find-room-response')
  app.ports.receiveRoomFoundResponse.send(data)
})

socket.on('join-room-response', data => {
  console.log('join-room-response')
  app.ports.receiveJoinRoomResponse.send(data)
})

socket.on('triplet-update', data => {
  console.log('triplet-update')
  app.ports.receiveUpdateTriplet.send(data)
})

socket.on('guest-joined', data => {
  console.log('guest-joined')
  app.ports.receiveGuestJoined.send(data)
})

socket.on('guest-left', data => {
  console.log('guest-left')
  app.ports.receiveGuestLeft.send(data)
})

socket.on('host-left', data => {
  console.log('host-left')
  app.ports.receiveHostLeft.send(data)
})

socket.on('kick-guest', data => {
  console.log('kick-guest')
  app.ports.receiveKickGuest.send(data)
})
