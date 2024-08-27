def main : IO Unit := do
  let englishGreeting := IO.println "Hello!" -- Será avaliada
  IO.println "Bonjour!" -- Está sendo Executada
  englishGreeting -- Está sendo Executada
