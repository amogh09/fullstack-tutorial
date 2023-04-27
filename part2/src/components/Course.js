const Part = ({ part }) =>
  <p>{part.name} {part.exercises}</p>

const Course = ({ course }) =>
  <div>
    <h2>{course.name}</h2>
    {course.parts.map(part => <Part key={part.id} part={part} />)}
    <p>total of {course.parts.reduce((s, p) => s + p.exercises, 0)} exercises</p>
  </div>

export default Course
