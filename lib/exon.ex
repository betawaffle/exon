defmodule Exon do
  def decode(json) do
    Exon.Decoder.decode(json)
  end
end
