const express = require('express')
const app = express()
const server = require('http').Server(app)
const io = require('socket.io')(server)
const low = require('lowdb')
const FileSync = require('lowdb/adapters/FileSync')

const db = low(new FileSync('.data/db.json'))

db.set('sessions', []).write()

function staticRoute(app, route, path) {
  app.get(route, function(_, response) {
    response.sendFile(__dirname + path)
  })
}

staticRoute(app, '/', '/index.html')
staticRoute(app, '/room/new', '/index.html')
staticRoute(app, '/room/:roomId/join', '/index.html')
staticRoute(app, '/room/:roomId', '/index.html')
staticRoute(app, '/room/:roomId/host', '/index.html')
staticRoute(app, '/fake/room/:roomId', '/index.html')
staticRoute(app, '/client.js', '/client.js')
staticRoute(app, '/elm.js', '/elm.js')
staticRoute(app, '/style.css', '/style.css')

const port = process.env.PORT || 5000
server.listen(port, () => {
  console.log(`Listening on port ${port}`)
})

function isAlreadyHost(socket) {
  const sessionWithSocketAsHost = db
    .get('sessions')
    .find({ hostId: socket.id })
    .value()

  return sessionWithSocketAsHost != undefined
}

function roomIdExists(roomId) {
  const sessionWithRoomId = db
    .get('sessions')
    .find({ roomId })
    .value()

  return sessionWithRoomId != undefined
}

function getRoom(roomId) {
  return db
    .get('sessions')
    .find({ roomId })
    .value()
}

function nameAlreadyExistsInRoom(name, roomId) {
  const userWithName = db
    .get('sessions')
    .find({ roomId })
    .get('guests')
    .find({ name })
    .value()
  return userWithName != undefined
}

function createRandomRoomWithHost(socket, config) {
  let roomId = '82'
  while (roomIdExists(roomId)) {
    roomId = Math.random()
      .toString()
      .substring(2, 8)
  }
  db.get('sessions')
    .push({ roomId, config, hostId: socket.id, guests: [] })
    .write()

  return roomId
}

function removeRoom(roomId) {
  db.get('sessions')
    .remove({ roomId: roomId })
    .write()
}

function addGuestToRoom(socketId, user, roomId) {
  db.get('sessions')
    .find({ roomId })
    .get('guests')
    .push({ socketId, ...user })
    .write()
}
function getGuest(socketId, roomId) {
  return db
    .get('sessions')
    .find({ roomId })
    .get('guests')
    .find({ socketId })
    .value()
}
function removeGuestFromRoom(socketId, roomId) {
  db.get('sessions')
    .find({ roomId })
    .get('guests')
    .remove({ socketId })
    .write()
}

function addHandlers(socket, roomId) {
  const room = {
    id: roomId,
    name: `room-${roomId}`,
    hostId: getRoom(roomId).hostId,
  }
  socket.join(room.name)

  socket.on('triplet-update', data => {
    socket.to(room.name).emit('triplet-update', data)
  })

  socket.on('kick-guest', data => {
    socket.to(room.name).emit('kick-guest', data)
  })

  socket.on('disconnect', () => {
    if (room.hostId === socket.id) {
      socket.to(room.name).emit('host-left')
      removeRoom(roomId)
    } else {
      socket
        .to(room.hostId)
        .emit('guest-left', { ok: { user: getGuest(socket.id, roomId) } })
      removeGuestFromRoom(socket.id, roomId)
    }
  })
}

io.on('connection', socket => {
  socket.on('create-room', ({ config }) => {
    if (!isAlreadyHost(socket)) {
      const roomId = createRandomRoomWithHost(socket, config)
      addHandlers(socket, roomId)
      socket.emit('create-room-response', {
        ok: { roomId },
      })
    } else {
      socket.emit('create-room-response', {
        error: 'already-host',
      })
    }
  })

  socket.on('join-room', ({ roomId, user }) => {
    if (!roomIdExists(roomId)) {
      socket.emit('join-room-response', {
        error: { type: 'no-room-with-id', payload: roomId },
      })
    } else if (nameAlreadyExistsInRoom(user.name, roomId)) {
      socket.emit('join-room-response', {
        error: { type: 'existing-username', payload: user.name },
      })
    } else {
      addHandlers(socket, roomId)
      addGuestToRoom(socket.id, user, roomId)
      const room = getRoom(roomId)
      const config = room.config
      socket.to(room.hostId).emit('guest-joined', {
        ok: { user },
      })
      socket.emit('join-room-response', {
        ok: { config },
      })
    }
  })
})
