import axios from "axios";

export const getNotes = () =>
  axios.get('/notes').then(res => res.data)

export const createNote = newNote =>
  axios.post('/notes', newNote).then(res => res.data)

export const updateNote = updatedNote =>
  axios.put(`/notes/${updatedNote.id}`, updatedNote).then(res => res.data)
