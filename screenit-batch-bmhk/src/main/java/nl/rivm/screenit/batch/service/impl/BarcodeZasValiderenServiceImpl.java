package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.KoppelConstants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.service.BarcodeValiderenService;
import nl.rivm.screenit.dao.BaseHoudbaarheidDao;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixZasHoudbaarheid;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.BaseHoudbaarheidService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixBaseMonsterService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Service;

import generated.KOPPELDATA;

@Service
@AllArgsConstructor
public class BarcodeZasValiderenServiceImpl extends BaseValiderenService implements BarcodeValiderenService
{
	private final BaseHoudbaarheidService houdbaarheidService;

	private final BaseHoudbaarheidDao houdbaarheidDao;

	private final CervixBaseMonsterService monsterService;

	private final HibernateService hibernateService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	public List<String> voerSemantischeValiatieUit(List<KOPPELDATA.VERZONDENUITNODIGING> koppeldata)
	{
		List<String> foutmeldingen = new ArrayList<>();
		List<String> barcodesUitKoppeldata = new ArrayList<>();

		var vandaag = currentDateSupplier.getLocalDate();
		var minstensHoudbaarTotMetCervix = houdbaarheidService.getMinstensHoudbaarTotMet(vandaag, PreferenceKey.PERIODE_MINIMALE_HOUDBAARHEID_ZAS_MONSTERS_VOOR_CONTROLE);

		for (KOPPELDATA.VERZONDENUITNODIGING verzondenUitnodiging : koppeldata)
		{

			String zasBarcode = getMatchingFieldValue(verzondenUitnodiging, KoppelConstants.CERVIX_KOPPEL_BARCODE_ZAS);
			String trackTraceId = getMatchingFieldValue(verzondenUitnodiging, KoppelConstants.KOPPEL_TRACK_ID);

			CervixUitnodiging cervixUitnodiging = getCervixUitnodiging(verzondenUitnodiging);

			if (cervixUitnodiging == null)
			{
				addFout(foutmeldingen, String.format(KoppelConstants.CERVIX_UITNODIGINGSID_ONBEKEND, verzondenUitnodiging.getID(), zasBarcode, trackTraceId));
			}
			else
			{
				if (StringUtils.isBlank(zasBarcode))
				{
					addFout(foutmeldingen,
						String.format(KoppelConstants.CERVIX_ZASID_MIST_BIJ_TYPE_UITNODIGING, verzondenUitnodiging.getID(),
							StringUtils.defaultIfBlank(zasBarcode, "<geen>"),
							trackTraceId, ""));
				}
				if (StringUtils.isNotBlank(zasBarcode))
				{
					var bestaandeZas = monsterService.getZas(zasBarcode).orElse(null);
					if (bestaandeZas != null && !bestaandeZas.getUitnodiging().equals(cervixUitnodiging))
					{
						addFout(foutmeldingen, String.format(KoppelConstants.CERVIX_ZASID_AL_GEKOPPELD, verzondenUitnodiging.getID(),
							StringUtils.defaultIfBlank(zasBarcode, "<geen>"), trackTraceId, bestaandeZas.getUitnodiging().getUitnodigingsId()));
					}
					if (cervixUitnodiging.getMonster() != null && !cervixUitnodiging.getMonster().getMonsterId().equals(zasBarcode))
					{
						addFout(foutmeldingen, String.format(KoppelConstants.CERVIX_UITNODIGINGSID_AL_GEKOPPELD, verzondenUitnodiging.getID(), zasBarcode, trackTraceId,
							cervixUitnodiging.getMonster().getMonsterId()));
					}
					CervixZasHoudbaarheid houdbaarheid = houdbaarheidDao.getHoudbaarheidVoor(CervixZasHoudbaarheid.class, zasBarcode);
					if (houdbaarheid == null)
					{
						addFout(foutmeldingen, String.format(KoppelConstants.CERVIX_HOUDBAARHEID_ONBEKEND, verzondenUitnodiging.getID(),
							StringUtils.defaultIfBlank(zasBarcode, "<geen>"), trackTraceId));
					}
					else if (DateUtil.toLocalDate(houdbaarheid.getVervalDatum()).isBefore(minstensHoudbaarTotMetCervix))
					{
						addFout(foutmeldingen, String.format(KoppelConstants.CERVIX_HOUDBAARHEID_TE_KORT, verzondenUitnodiging.getID(),
							StringUtils.defaultIfBlank(zasBarcode, "<geen>"), trackTraceId));
					}
					if (barcodeAlTeruggekoppeld(barcodesUitKoppeldata, zasBarcode))
					{
						addFout(foutmeldingen, String.format(KoppelConstants.CERVIX_ZASID_IS_DUBBEL, verzondenUitnodiging.getID(), zasBarcode, trackTraceId));
					}
					else
					{
						barcodesUitKoppeldata.add(zasBarcode);
					}
				}
			}
		}
		return foutmeldingen;
	}

	private CervixUitnodiging getCervixUitnodiging(KOPPELDATA.VERZONDENUITNODIGING verzondenUitnodiging)
	{
		HashMap<String, Long> parameters = new HashMap<>();
		parameters.put("uitnodigingsId", verzondenUitnodiging.getID());
		return hibernateService.getUniqueByParameters(CervixUitnodiging.class, parameters);
	}

	@Override
	protected void logKoppelenFout(LogEvent logEvent)
	{
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_ZAS_CONTROLE_KOPPELEN_FOUT, logEvent, Bevolkingsonderzoek.CERVIX);
	}
}
