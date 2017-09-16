var app = require('express')()
var server = require('http').Server(app)
var io = require('socket.io')(server)

server.listen(5000)

let hostId


io.on('connection', socket => {
  
  socket.on('connect-as-host', () => {
    if (hostId == undefined) {
      hostId = socket.id
      socket.emit('connect-as-host-response', {ok: ''})

      socket.on('disconnect', () => {
        hostId = undefined
      })
    } else {
      socket.emit('connect-as-host-response', {error: "There's already a host"})
    }
  })

  socket.on('request-topping-list', data => {
    socket.to(hostId).emit('request-topping-list', data)
  })

  socket.on('topping-list', data => {
    socket.broadcast.emit('topping-list', data)
  })

  socket.on('triplet', data => {
    socket.broadcast.emit('triplet', data)
  })
})
