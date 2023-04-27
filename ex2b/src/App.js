import { useState, useEffect } from 'react'
import Person from './components/Person'
import phonebookService from './services/phonebook'

const Filter = ({ nameFilter, handleNameFilterChange }) =>
  <div>
    filter shown with
    <input value={nameFilter} onChange={handleNameFilterChange} />
  </div>

const PersonForm = ({ addName, newName, handleNameChange, newNumber, handleNumberChange }) =>
  <form onSubmit={addName}>
    <div>name: <input value={newName} onChange={handleNameChange} /></div>
    <div>number: <input value={newNumber} onChange={handleNumberChange} /></div>
    <div><button type="submit">add</button></div>
  </form>

const Success = ({ message }) => {
  if (message === null) {
    return null
  }

  const style = {
    color: 'green',
    fontSize: 16,
    borderStyle: 'solid',
    borderRadius: 5,
    padding: 10,
    background: 'lightgrey',
    marginBottom: 12
  }

  return (
    <div style={style}>
      {message}
    </div>
  )
}

const Error = ({ message }) => {
  if (message === null) {
    return null
  }

  const style = {
    color: 'red',
    fontSize: 16,
    borderStyle: 'solid',
    borderRadius: 5,
    padding: 10,
    background: 'lightgrey',
    marginBottom: 12
  }

  return (
    <div style={style}>
      {message}
    </div>
  )
}

const App = () => {
  const [persons, setPersons] = useState([])
  const [newName, setNewName] = useState('')
  const [newNumber, setNewNumber] = useState('')
  const [nameFilter, setNameFilter] = useState('')
  const [successMessage, setSuccessMessage] = useState(null)
  const [errorMessage, setErrorMessage] = useState(null)

  useEffect(() => { phonebookService.getAll().then(ps => setPersons(ps)) }, [])

  const handleNameChange = (event) => { setNewName(event.target.value) }
  const handleNumberChange = (event) => { setNewNumber(event.target.value) }
  const handleNameFilterChange = (event) => { setNameFilter(event.target.value) }

  const updateExisting = (id, newPerson) => {
    phonebookService
      .update(id, newPerson)
      .then(updatedPerson =>
        setPersons(persons.map(p => p.id !== id ? p : updatedPerson))
      )
      .catch((_error) => {
        setErrorMessage(`Information for '${newPerson.name}' has already been deleted from the server.`)
        setTimeout(() => {setErrorMessage(null)}, 5000)
      })
  }

  const addName = (event) => {
    event.preventDefault()
    const existing = persons.find(p => p.name === newName)
    if (existing) {
      updateExisting(existing.id, { ...existing, number: newNumber })
    } else {
      const newPerson = {
        name: newName,
        number: newNumber
      }
      phonebookService
        .create(newPerson)
        .then(p => {
          setPersons(persons.concat(p))
          setSuccessMessage(`'${newName}' added successfully.`)
          setTimeout(() => { setSuccessMessage(null) }, 5000)
        })
    }
    setNewName('')
    setNewNumber('')
  }

  const delPerson = (id) => {
    if (window.confirm("Do you want to delete this person?")) {
      phonebookService.del(id).then(setPersons(persons.filter(p => p.id !== id)))
    }
  }

  const filteredPersons =
    nameFilter === ''
      ? persons
      : persons.filter(p => p.name.startsWith(nameFilter))

  return (
    <div>
      <h2>Phonebook</h2>
      <Success message={successMessage} />
      <Error message={errorMessage} />
      <Filter nameFilter={nameFilter} handleNameFilterChange={handleNameFilterChange} />
      <h2>add a new</h2>
      <PersonForm
        addName={addName}
        newName={newName} handleNameChange={handleNameChange}
        newNumber={newNumber} handleNumberChange={handleNumberChange}
      />
      <h2>Numbers</h2>
      {filteredPersons.map(p => <Person
        key={p.id} person={p} delPerson={() => delPerson(p.id)}
      />)}
    </div>
  )
}

export default App
