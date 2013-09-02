defmodule Exon.Mixfile do
  use Mix.Project

  def project do
    [app: :exon,
     version: "0.0.1",
     elixir: "~> 0.10.2-dev",
     deps: deps]
  end

  def application do
    []
  end

  defp deps do
    []
  end
end
