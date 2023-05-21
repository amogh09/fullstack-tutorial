import { createSlice } from "@reduxjs/toolkit"
import noteService from '../services/notes'

const noteSlice = createSlice({
  name: 'notes',
  initialState: [],
  reducers: {
    toggleImportanceOf(state, action) {
      const id = action.payload
      const noteToChange = state.find(n => n.id === id)
      const changedNote = { ...noteToChange, important: !noteToChange.important }
      return state.map(n => n.id !== id ? n : changedNote)
    },
    appendNote(state, action) {
      return [...state, action.payload]
    },
    setNotes(_state, action) {
      return action.payload
    }
  }
})

export const { toggleImportanceOf, appendNote, setNotes } = noteSlice.actions

export const initializeNotes = () => {
  return async dispatch => {
    const notes = await noteService.getAll()
    dispatch(setNotes(notes))
  }
}

export const createNote = content => {
  return async dispatch => {
    noteService.createNew(content).then(note => dispatch(appendNote(note)))
  }
}

export default noteSlice.reducer
