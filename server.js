const express = require('express')
const app = express()
const server = require('http').Server(app)
const io = require('socket.io')(server)
const low = require('lowdb')
const FileSync = require('lowdb/adapters/FileSync')

const db = low(new FileSync('.data/db.json'))

db.set('sessions', [])
  .write()


app.use(express.static('client'))

app.get("/", function (request, response) {
  response.sendFile(__dirname + '/client/index.html')
})

const port = process.env.PORT | 5000
server.listen(port, () => {
  console.log(`Listening on port ${port}`)
})


function alreadyHost(socket) {
  const sessionWithSocketAsHost = db.get('sessions')
    .find({ hostId: socket.id })
    .value()

  return sessionWithSocketAsHost != undefined
}

function roomIdExists(roomId) {
  const sessionWithRoomId = db.get('sessions')
    .find({ roomId })
    .value()
  
    return sessionWithRoomId != undefined
}

function hostOfRoom(roomId) {
  const hostId = db.get('sessions')
    .find({ roomId })
    .value()
    .hostId

  return hostId
}

function createRandomRoomWithHost(socket) {
  const roomId = Math.random().toString().substring(2, 8)
  db.get('sessions')
    .push({ roomId, hostId: socket.id })
    .write()

  return roomId
}

function joinRoomAndAddHandlers(socket, roomId) {
  const roomName = `room-${roomId}`
  socket.join(roomName)
  const hostId = hostOfRoom(roomId)

  socket.on('request-topping-list', data => {
    socket.to(hostId).emit('request-topping-list', data)
  })

  socket.on('topping-list', data => {
    socket.to(roomName).emit('topping-list', data)
  })

  socket.on('triplet', data => {
    socket.to(roomName).emit('triplet', data)
  })

  socket.on('disconnect', () => {
    db.get('sessions')
      .remove({ hostId: socket.id })
      .write()
  })
}


io.on('connection', socket => {

  socket.on('create-room', () => {
    
    if (!alreadyHost(socket)) {
      const roomId = createRandomRoomWithHost(socket)
      joinRoomAndAddHandlers(socket, roomId)
      socket.emit('create-room-response', { ok: { roomId } })
    } else {
      socket.emit('create-room-response', 
        { error: "The user is already a host" }
      )
    }
  })

  socket.on('join-room', roomId => {
    if (roomIdExists(roomId)) {
      joinRoomAndAddHandlers(socket, roomId)
      socket.emit('join-room-response', { ok: roomId })
    } else {
      socket.emit('join-room-response', 
        { error: "There's no room with this id" }
      )
    }
  })
})
  