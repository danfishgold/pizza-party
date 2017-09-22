const express = require('express')
const app = express()
const server = require('http').Server(app)
const io = require('socket.io')(server)

app.use(express.static('client'))

app.get("/", function (request, response) {
  response.sendFile(__dirname + '/client/index.html')
})


server.listen(process.env.PORT)

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