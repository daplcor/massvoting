(module dao GOV

; Capabilities
(defcap GOV ()
  (enforce-keyset "DAO_NS.governance" ))

(defcap BOT()
  (enforce-keyset "DAO_NS.bot"))

; Event Trackers 

(defcap VOTER (voter:string proposalId:string vote:bool)
  @event true)  

(defcap PROPOSAL (proposalId:string question:string description:string channelName:string channelNumber:integer creator:string startTime:time endTime:time votesFor:integer votesAgainst:integer quorum:integer)
  @event true)

; Schemas

(defschema vote-schema
  @doc "Key valie is concat of proposalId and voter"
    proposalId:string
    voter:string
    vote:bool
    time:time)

(defschema proposal
  @doc "Key is proposalId"
    proposalId:string
    question:string
    channelName:string
    channelNumber:integer
    description:string
    creator:string
    startTime:time
    endTime:time
    votesFor:integer 
    votesAgainst:integer
    quorum:integer)

(defschema channel-stats
  @doc "Key is a hash of channelName"
    channelName:string
    totalProposals:integer
    totalVotes:integer
    activeProposals:integer
    lastProposalTime:time)

(deftable votes:{vote-schema})
(deftable channels:{channel-stats})
(deftable proposals:{proposal})

; Initiator Functions

(defun create-proposal:string
    (
        question:string 
        description:string
        channelName:string 
        channelNumber:integer
        start:time 
        end:time 
        creator:string
        quorum:integer
        )
   @doc "Creates a proposal to vote on"
   
    ; Enforces strings are not empty and meet minimum length
    (validate-string question STRLENGTH)
    (validate-string description STRLENGTH)
    (validate-string channelName STRLENGTH)
    (validate-string creator 3)

    (enforce (>= quorum 0) "Quorum must be greater than 0")
    (enforce (> end start) "End time must be greater than start time")
    
    (let ((id:string (hash-id question channelName (curr-time))))
    (with-capability (BOT)
      (insert proposals id 
        {
        "proposalId": id,
        "question": question,
        "description": description,
        "channelName": channelName,
        "channelNumber": channelNumber,
        "creator": creator,
        "startTime": start,
        "endTime": end,
        "votesFor": 0,
        "votesAgainst": 0,
        "quorum": quorum
        }
      )
    )

    (let ((hashed-name (hash-channel channelName))) 
    (with-default-read channels hashed-name 
      {'totalProposals: 0, 'totalVotes: 0, 'activeProposals: 0, 'lastProposalTime: (curr-time)} 
      {'totalProposals:=totalProposals, 'totalVotes:=totalVotes, 'activeProposals:=activeProposals, 'lastProposalTime:=lastProposalTime}
      (write channels hashed-name {"channelName": channelName, "totalProposals": (+ totalProposals 1), "totalVotes": totalVotes, "activeProposals": (+ activeProposals 1), "lastProposalTime": (curr-time) })
    ))
      (format "Proposal {} created" [id])
  )
)

; Interactive Functions

(defun place-vote:string (proposalId:string vote:bool voter:string)
  (with-read proposals proposalId {'channelName:= channelName, 'startTime:=startTime, 'endTime:=endTime, 'votesFor:=votesFor, 'votesAgainst:=votesAgainst}
    (enforce (> (curr-time) startTime) "Voting has not started yet")
    (enforce (< (curr-time) endTime) "Voting has ended")    
    (enforce (!= voter "") "Voter cannot be empty")
       (with-capability (BOT)
        ; Inserts individual vote stats, will fail if voter has already voted
        (insert-voter-info proposalId voter vote)
        
        ; Updates proposal stats, protected by BOT capability
        (update-proposal-info proposalId vote votesFor votesAgainst)
        
        ; Updates channel stats if all checks pass
        (with-read channels (hash-channel channelName) {'totalVotes:=totalVotes}
          (update channels (hash-channel channelName) { "totalVotes": (+ totalVotes 1)})
        )
       )
  )
  (format "{} voted {} on proposal {}" [voter, vote, proposalId])
)

; Internal Functions

(defun insert-voter-info:string (proposalId:string voter:string vote:bool)
  @doc "Inserts voter info"
  (require-capability (BOT))
    (insert votes (concat-votes proposalId voter) { "proposalId": proposalId, "voter": voter, 
            "vote": vote, "time": (curr-time) }))

(defun update-proposal-info:string (proposalId:string vote:bool votesFor:integer votesAgainst:integer)
  @doc "Updates proposal info"
  (require-capability (BOT))
    (update proposals proposalId { "votesFor": (if (= vote true) (+ votesFor 1) votesFor), 
            "votesAgainst": (if (= vote false) (+ votesAgainst 1) votesAgainst) }))

; Tally Functions

(defun check-proposal-status:object (proposalId:string)
    @doc "Returns the status of a proposal including if it passed/failed and voting stats"
    (with-read proposals proposalId
        { 'votesFor := votesFor
        , 'votesAgainst := votesAgainst
        , 'quorum := quorum
        , 'endTime := endTime
        , 'startTime := startTime
        }
        
        (let*
            (
                (total-votes (+ votesFor votesAgainst))
                (current-time (curr-time))
                (voting-ended (> current-time endTime))
                (voting-started (> current-time startTime))
                (quorum-met (or (= quorum 0) (>= total-votes quorum)))
                (more-votes-for (> votesFor votesAgainst))
            )
            
            {
                "proposalId": proposalId,
                "status": (cond
                  ((not voting-started) "NOT_STARTED")
                  ((not voting-ended) "IN_PROGRESS")
                  ((not quorum-met) "FAILED_QUORUM")
                  (more-votes-for "PASSED")
                  "FAILED"
                ),
                "votesFor": votesFor,
                "votesAgainst": votesAgainst,
                "totalVotes": total-votes,
                "quorum": quorum,
                "quorumMet": quorum-met,
                "votingEnded": voting-ended,
                "votingStarted": voting-started,
                "endTime": endTime,
                "currentTime": current-time
            }
        )
    )
)

; Constants

(defconst STRLENGTH:integer 4)

; Helper Functions

(defun validate-string:bool (str:string min-length:integer)
  @doc "Validates that a string is not empty and meets minimum length"
  (enforce (!= str "") "String cannot be empty")
  (enforce (>= (length str) min-length) 
    (format "String must be at least {} characters" [min-length]))
  true)

(defun concat-votes:string (proposalId:string voter:string)
  (format "{}:{}" [proposalId, voter]))

(defun hash-id:string
  (question:string channelName:string times:time)
  (format "d:{}" [(hash {'q:question, "cn":channelName, "time":times})]))

(defun get-channel-stats:object (channelName:string)
  (read channels (hash-channel channelName)))

(defun hash-channel:string (channelName:string)
  (hash channelName))

(defun fun ()
  (select proposals (constantly true)))

(defun curr-time:time ()
  @doc "Returns current chain's block-time"
  (at 'block-time (chain-data)))
)

(create-table proposals)
(create-table votes)
(create-table channels)
