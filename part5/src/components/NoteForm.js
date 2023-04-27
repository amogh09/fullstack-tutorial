import { useState } from 'react'

const NoteForm = ({ createNote }) => {
  const [newNote, setNewNote] = useState('')

  const addNote = (event) => {
    event.preventDefault()
    createNote({ content: newNote, important: false })
    setNewNote('')
  }

  return (
    <div>
      <h2>Create a new note</h2>
      <form onSubmit={addNote}>
        <input
          value={newNote}
          placeholder='write a new note here'
          onChange={event => setNewNote(event.target.value)}
        />
        <button type="submit">Save</button>
      </form>
    </div>
  )
}

export default NoteForm
