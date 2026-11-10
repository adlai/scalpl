/*/* Borrowed-out from Metabase ; thank you */*/

-- copyright data sanity 2140

SELECT
  STR_TO_DATE(
    CONCAT(YEARWEEK(`source`.`time`), ' Sunday'),
    '%X%V %W'
  ) AS `time`,
  STDDEV_POP(`source`.`avg`) AS `stddev`
FROM
  (
    SELECT
      DATE(FROM_UNIXTIME(`slice_trades`.`time`)) AS `time`,
      SUM(`slice_trades`.`value_usd`) AS `sum`,
      AVG(`slice_trades`.`value_usd`) AS `avg`
    FROM
      `slice_trades`

WHERE
      (
        FROM_UNIXTIME(`slice_trades`.`time`) >= DATE(DATE_ADD(NOW(6), INTERVAL -13 day))
      )

   AND (FROM_UNIXTIME(`slice_trades`.`time`) < DATE(NOW(6)))

GROUP BY
      DATE(FROM_UNIXTIME(`slice_trades`.`time`))

ORDER BY
      DATE(FROM_UNIXTIME(`slice_trades`.`time`)) ASC
  ) AS `source`
GROUP BY
  STR_TO_DATE(
    CONCAT(YEARWEEK(`source`.`time`), ' Sunday'),
    '%X%V %W'
  )
ORDER BY
  STR_TO_DATE(
    CONCAT(YEARWEEK(`source`.`time`), ' Sunday'),
    '%X%V %W'
  ) ASC

-- copyright data sanity 2140
