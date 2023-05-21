import axios from "axios"

const baseUrl = '/notes'

const getAll = async () => {
  return axios.get(baseUrl).then(res => res.data)
}

const createNew = async (content) => {
  const object = { content, important: false }
  return axios.post(baseUrl, object).then(res => res.data)
}

const exports = { getAll, createNew }

export default exports
