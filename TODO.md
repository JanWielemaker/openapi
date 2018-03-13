# OpenAPI plans

  - Server
    - Handle errors
    - Integrate OpenAPI GUI tools				[OK]
    - Make some of the type checking controlled by options
    - Generate the implementation templates			[OK]
      - Predicate head
      - PlDoc comment

  - Client
    - Generate client predicates from spec			[OK]
      - Normal query parameters					[OK]
      - Path parameters						[OK]
      - Request body						[OK]
    - Use an option list for optional arguments?		[OK]
    - Generate PlDoc.						[OK]
    - Provide reflexive type interface.
    - Type checking
      - request							[OK]
      - response
        - Deal with empty "OK" responses
	  - 201
	    - May read result from `Location` header
	    - May return the object proper

  - Testing
    - Is there a comprehensive test suite?

  - Use URI encode/decode for path parameters

## Open issues

  - How to select a server?  Use with other servers?  Option?
