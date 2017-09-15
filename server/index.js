var app = require('express')()
var server = require('http').Server(app)
var io = require('socket.io')(server)

server.listen(5000)

let hostId

io.on('connection', socket => {
  if (hostId == undefined) {
    hostId = socket.id
  }
  socket.on('request-all-topping-counts', data => {
    socket.to(hostId).emit('request-all-topping-counts', data)
  })

  socket.on('send-all-topping-counts', data => {
    socket.broadcast.emit('send-all-topping-counts', data)
  })

  socket.on('topping-triplet', data => {
    socket.broadcast.emit('topping-triplet', data)
  })
})
