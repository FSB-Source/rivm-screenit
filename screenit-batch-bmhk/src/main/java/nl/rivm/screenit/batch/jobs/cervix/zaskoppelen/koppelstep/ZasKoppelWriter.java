package nl.rivm.screenit.batch.jobs.cervix.zaskoppelen.koppelstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.NonUniqueResultException;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.KoppelConstants;
import nl.rivm.screenit.batch.jobs.cervix.zaskoppelen.ZasKoppelenConstants;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.logging.ZasKoppelingBeeindigdLogEvent;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import generated.KOPPELDATA.VERZONDENUITNODIGING;
import generated.KOPPELDATA.VERZONDENUITNODIGING.MATCHINGFIELDS.MATCHINGFIELD;

@Component
@Slf4j
public class ZasKoppelWriter implements ItemWriter<VERZONDENUITNODIGING>
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private CervixFactory cervixFactory;

	private StepExecution stepExecution;

	private ZasKoppelingBeeindigdLogEvent logEvent;

	@Override
	public void write(List<? extends VERZONDENUITNODIGING> items) throws Exception
	{
		logEvent = (ZasKoppelingBeeindigdLogEvent) stepExecution.getJobExecution().getExecutionContext().get(ZasKoppelenConstants.RAPPORTAGEKEYZASKOPPELEN);

		var verzendDatumFormat = new SimpleDateFormat("dd-MM-yyyy");
		for (VERZONDENUITNODIGING verzondenUitnodiging : items)
		{

			String zasBarcode = null;
			String trackTraceId = null;

			try
			{

				zasBarcode = getMatchingFieldValue(verzondenUitnodiging, KoppelConstants.CERVIX_KOPPEL_BARCODE_ZAS, true);
				trackTraceId = getMatchingFieldValue(verzondenUitnodiging, KoppelConstants.KOPPEL_TRACK_ID, false);

				Map<String, Long> parameters = new HashMap<String, Long>();
				parameters.put("uitnodigingsId", verzondenUitnodiging.getID());
				var cervixUitnodiging = hibernateService.getUniqueByParameters(CervixUitnodiging.class, parameters);

				cervixUitnodiging.setVerstuurdDoorInpakcentrum(true);
				cervixUitnodiging.setTrackTraceId(trackTraceId);

				if (StringUtils.isNotBlank(zasBarcode))
				{
					CervixZas zas = CervixMonsterUtil.getZAS(cervixUitnodiging.getMonster());
					if (zas != null && !zas.getMonsterId().equals(zasBarcode))
					{
						zas = null;
					}
					if (zas == null)
					{
						zas = cervixFactory.maakZasMonster(cervixUitnodiging, zasBarcode);
					}
					Date datumVerstuurd = verzendDatumFormat.parse(getMatchingFieldValue(verzondenUitnodiging, KoppelConstants.KOPPEL_DATUM_VERZENDING, true));
					zas.setVerstuurd(datumVerstuurd);

					hibernateService.saveOrUpdate(zas);
				}

				hibernateService.saveOrUpdate(cervixUitnodiging);

				logEvent.setAantalZasVerwerkt(logEvent.getAantalZasVerwerkt() + 1);
			}
			catch (ParseException | NonUniqueResultException e)
			{
				LOG.error("Fout bij verwerken van koppel data regel ", e);
				var melding = String.format(KoppelConstants.CERVIX_ONBEKENDE_FOUT, verzondenUitnodiging.getID(), StringUtils.defaultIfBlank(zasBarcode, "<geen>"),
					StringUtils.defaultIfBlank(trackTraceId, "<geen>"), e.getMessage());

				logEvent.setLevel(Level.ERROR);
				logEvent.setMelding(melding);
				throw new IllegalStateException(melding);
			}
		}
	}

	private String getMatchingFieldValue(VERZONDENUITNODIGING verzondenUitnodiging, String matchingFieldName, boolean required)
	{
		for (MATCHINGFIELD matchingField : verzondenUitnodiging.getMATCHINGFIELDS().getMATCHINGFIELD())
		{
			if (matchingField.getNAME().equalsIgnoreCase(matchingFieldName))
			{
				return matchingField.getVALUE();
			}
		}

		if (required)
		{
			logEvent.setLevel(Level.ERROR);
			logEvent.setMelding("Het bericht had niet alle gegevens beschikbaar - MatchingField: " + matchingFieldName);
			throw new IllegalStateException("MatchingField not found: " + matchingFieldName);
		}
		return null;
	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
	}

	public void setHibernateService(HibernateService hibernateService)
	{
		this.hibernateService = hibernateService;
	}

}
