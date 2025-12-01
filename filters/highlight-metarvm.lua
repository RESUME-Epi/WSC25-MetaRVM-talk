function Str(elem)
  if elem.text == "MetaRVM" then
    return pandoc.Span("MetaRVM", {class="metarvm"})
  end
end