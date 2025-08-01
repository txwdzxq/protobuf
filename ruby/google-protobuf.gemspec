Gem::Specification.new do |s|
  s.name        = "google-protobuf"
  s.version     = "4.33.0"
  git_tag       = "v#{s.version.to_s.sub('.rc.', '-rc')}" # Converts X.Y.Z.rc.N to vX.Y.Z-rcN, used for the git tag
  s.licenses    = ["BSD-3-Clause"]
  s.summary     = "Protocol Buffers"
  s.description = "Protocol Buffers are Google's data interchange format."
  s.homepage    = "https://developers.google.com/protocol-buffers"
  s.authors     = ["Protobuf Authors"]
  s.email       = "protobuf@googlegroups.com"
  s.metadata    = { "source_code_uri" => "https://github.com/protocolbuffers/protobuf/tree/#{git_tag}/ruby" }
  s.require_paths = ["lib"]
  s.files       = Dir.glob('lib/**/*.{rb,rake}')
  if RUBY_PLATFORM == "java"
    s.platform  = "java"
    s.files     += ["lib/google/protobuf_java.jar"] +
      Dir.glob('ext/**/*').reject do |file|
        File.basename(file) =~ /^((convert|defs|map|repeated_field)\.[ch]|
                                   BUILD\.bazel|extconf\.rb)$/x
      end
    s.extensions = ["ext/google/protobuf_c/Rakefile"]
    s.add_dependency "ffi", "~>1"
    s.add_dependency "ffi-compiler", "~>1"
  else
    s.files     += Dir.glob('ext/**/*').reject do |file|
      File.basename(file) =~ /^(BUILD\.bazel)$/
    end

    # When installing this gem from git via bundler
    # (ie: 'gem "google-protobuf", git: "https://.../protobuf.git"' in your
    # Gemfile), Rakefile is necessary so the prerequisite tasks run to copy
    # third party C libraries and generate well known protobufs.  When building
    # the gem via `rake gem`, these steps will have already occurred, and so we
    # replace the `Rakefile` extension with `ext/google/protobuf_c/extconf.rb`.
    # See the `Gem::PackageTask.new` declaration in `Rakefile` for more details.
    s.extensions = [
      File.exist?("Rakefile") ? "Rakefile" : "ext/google/protobuf_c/extconf.rb",
      "ext/google/protobuf_c/Rakefile"
    ]
  end
  s.required_ruby_version = '>= 3.1'
  # bigdecimal must be used as a non-built in gem as of ruby-3.4
  s.add_dependency "bigdecimal"
  # TODO: evaluate removing Rakefile and moving logic to extconf.rb, so that we
  # can remove this runtime dependency on rake. See the discussion here for
  # more details:
  # https://github.com/protocolbuffers/protobuf/pull/15203
  s.add_dependency "rake", ">= 13"
  s.add_development_dependency "ffi", "~>1"
  s.add_development_dependency "ffi-compiler", "~>1"
  s.add_development_dependency "rake-compiler", "~> 1.2"
  s.add_development_dependency "rake-compiler-dock", "~> 1.9"
  s.add_development_dependency "test-unit", '~> 3.0', '>= 3.0.9'
end
