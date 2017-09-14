var app = require('express')()
var server = require('http').Server(app)
var io = require('socket.io')(server)

server.listen(5000)

io.on('connection', socket => {
  socket.on('request-all-topping-counts', data => {
    socket.broadcast.emit('request-all-topping-counts', data)
  })

  socket.on('send-all-topping-counts', data => {
    socket.broadcast.emit('send-all-topping-counts', data)
  })

  socket.on('topping-triplet', data => {
    socket.broadcast.emit('topping-triplet', data)
  })
})
