const Person = ({ person, delPerson }) => {
  return (
    <div>
      {person.name} {person.number}
      <button onClick={delPerson}>Delete</button>
    </div>
  )
}

export default Person
