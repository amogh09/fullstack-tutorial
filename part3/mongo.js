const mongoose = require('mongoose')

const url = 'mongodb://localhost:27017/mydatabase'

mongoose.set('strictQuery', false)
mongoose.connect(url)

const noteSchema = new mongoose.Schema({
  content: String,
  important: Boolean
})

const Note = mongoose.model('Note', noteSchema)

const note = new Note({
  content: 'HTML is easy',
  important: true,
})

note.save().then(result => {
  console.log('Note saved')
  mongoose.connection.close()
})
