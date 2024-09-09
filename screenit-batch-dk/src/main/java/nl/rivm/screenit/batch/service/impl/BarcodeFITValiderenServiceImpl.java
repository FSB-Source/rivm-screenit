package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import nl.rivm.screenit.KoppelConstants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.service.BarcodeValiderenService;
import nl.rivm.screenit.dao.BaseHoudbaarheidDao;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.IFOBTVervaldatum;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.BaseHoudbaarheidService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonBaseFITService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import generated.KOPPELDATA;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class BarcodeFITValiderenServiceImpl extends BaseValiderenService implements BarcodeValiderenService
{
	@Autowired
	private BaseHoudbaarheidService houdbaarheidService;

	@Autowired
	private ColonBaseFITService fitService;

	@Autowired
	private BaseHoudbaarheidDao houdbaarheidDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public List<String> voerSemantischeValiatieUit(List<KOPPELDATA.VERZONDENUITNODIGING> koppeldata)
	{
		var foutmeldingen = new ArrayList<String>();
		var barcodesUitKoppeldata = new ArrayList<String>();

		var vandaag = currentDateSupplier.getLocalDate();
		var minstensHoudbaarTotMet = houdbaarheidService.getMinstensHoudbaarTotMet(vandaag, PreferenceKey.PERIODE_MINIMALE_HOUDBAARHEID_IFOBT_MONSTERS_VOOR_CONTROLE);

		koppeldata.forEach(koppelRegel -> valideerKoppelRegel(koppelRegel, foutmeldingen, minstensHoudbaarTotMet, barcodesUitKoppeldata));
		return foutmeldingen;
	}

	private void valideerKoppelRegel(KOPPELDATA.VERZONDENUITNODIGING koppelRegel, List<String> foutmeldingen, LocalDate minstensHoudbaarTotMetColon,
		List<String> barcodesUitKoppeldata)
	{
		var uitnodiging = getUitnodiging(koppelRegel);
		var onderzoeksVariant = getOnderzoeksVariant(uitnodiging);

		var barcodeGoldVerplicht = ColonOnderzoeksVariant.isOfType(onderzoeksVariant, IFOBTType.GOLD);
		var barcodeExtraVerplicht = ColonOnderzoeksVariant.isOfType(onderzoeksVariant, IFOBTType.STUDIE);

		var fitBarcodeGold = getMatchingFieldValue(koppelRegel, KoppelConstants.COLON_KOPPEL_BARCODE_GOLD);
		var fitBarcodeExtra = getMatchingFieldValue(koppelRegel, KoppelConstants.COLON_KOPPEL_BARCODE_EXTRA);
		var trackTraceId = getMatchingFieldValue(koppelRegel, KoppelConstants.KOPPEL_TRACK_ID);

		if (uitnodiging == null)
		{
			addFout(foutmeldingen, String.format(KoppelConstants.COLON_UITNODIGINGSID_ONBEKEND, koppelRegel.getID(), fitBarcodeGold,
				StringUtils.defaultIfBlank(fitBarcodeExtra, "<geen>"), trackTraceId));
		}
		else
		{
			if (barcodeGoldVerplicht && StringUtils.isBlank(fitBarcodeGold))
			{
				addFout(foutmeldingen, String.format(KoppelConstants.COLON_BUISID_MIST_BIJ_TYPE_UITNODIGING, koppelRegel.getID(),
					StringUtils.defaultIfBlank(fitBarcodeGold, "<geen>"), StringUtils.defaultIfBlank(fitBarcodeExtra, "<geen>"), trackTraceId, ""));
			}
			if (barcodeExtraVerplicht && StringUtils.isBlank(fitBarcodeExtra))
			{
				addFout(foutmeldingen, String.format(KoppelConstants.COLON_BUISID_MIST_BIJ_TYPE_UITNODIGING, koppelRegel.getID(),
					StringUtils.defaultIfBlank(fitBarcodeGold, "<geen>"), StringUtils.defaultIfBlank(fitBarcodeExtra, "<geen>"), trackTraceId, "Extra"));
			}
			if (!barcodeGoldVerplicht && StringUtils.isNotBlank(fitBarcodeGold))
			{
				addFout(foutmeldingen, String.format(KoppelConstants.COLON_BUISID_ONVERWACHT_TYPE_UITNODIGING, koppelRegel.getID(),
					StringUtils.defaultIfBlank(fitBarcodeGold, "<geen>"), StringUtils.defaultIfBlank(fitBarcodeExtra, "<geen>"), trackTraceId, ""));
			}
			if (!barcodeExtraVerplicht && StringUtils.isNotBlank(fitBarcodeExtra))
			{
				addFout(foutmeldingen, String.format(KoppelConstants.COLON_BUISID_ONVERWACHT_TYPE_UITNODIGING, koppelRegel.getID(),
					StringUtils.defaultIfBlank(fitBarcodeGold, "<geen>"), StringUtils.defaultIfBlank(fitBarcodeExtra, "<geen>"), trackTraceId, "Extra"));
			}
			if (StringUtils.isNotBlank(fitBarcodeGold))
			{
				fitService.getFit(fitBarcodeGold).ifPresent(bestaandeFITGold ->
				{
					if (IFOBTType.GOLD.equals(bestaandeFITGold.getType())
						&& !bestaandeFITGold.getColonUitnodiging().equals(uitnodiging))
					{
						addFout(foutmeldingen, String.format(KoppelConstants.COLON_BUISID_AL_GEKOPPELD, koppelRegel.getID(),
							StringUtils.defaultIfBlank(fitBarcodeGold, "<geen>"),
							StringUtils.defaultIfBlank(fitBarcodeExtra, "<geen>"), trackTraceId, "",
							bestaandeFITGold.getColonUitnodiging().getUitnodigingsId()));
					}
				});
				if (uitnodiging.getGekoppeldeTest() != null && !uitnodiging.getGekoppeldeTest().getBarcode().equals(fitBarcodeGold))
				{
					addFout(foutmeldingen, String.format(KoppelConstants.COLON_UITNODIGINGSID_AL_GEKOPPELD, koppelRegel.getID(), fitBarcodeGold,
						StringUtils.defaultIfBlank(fitBarcodeExtra, "<geen>"), trackTraceId, "", uitnodiging.getGekoppeldeTest().getBarcode()));
				}
				var ifobtVervaldatum = houdbaarheidDao.getHoudbaarheidVoor(IFOBTVervaldatum.class, fitBarcodeGold);
				if (ifobtVervaldatum == null)
				{
					addFout(foutmeldingen, String.format(KoppelConstants.COLON_HOUDBAARHEID_ONBEKEND, koppelRegel.getID(), fitBarcodeGold,
						StringUtils.defaultIfBlank(fitBarcodeExtra, "<geen>"), trackTraceId, ""));
				}
				else if (DateUtil.toLocalDate(ifobtVervaldatum.getVervalDatum()).isBefore(minstensHoudbaarTotMetColon))
				{
					addFout(foutmeldingen, String.format(KoppelConstants.COLON_HOUDBAARHEID_TE_KORT, koppelRegel.getID(), fitBarcodeGold,
						StringUtils.defaultIfBlank(fitBarcodeExtra, "<geen>"), trackTraceId, ""));
				}
				if (barcodeAlTeruggekoppeld(barcodesUitKoppeldata, fitBarcodeGold))
				{
					addFout(foutmeldingen, String.format(KoppelConstants.COLON_BUISID_IS_DUBBEL, koppelRegel.getID(), fitBarcodeGold,
						StringUtils.defaultIfBlank(fitBarcodeExtra, "<geen>"), trackTraceId));
				}
				else
				{
					barcodesUitKoppeldata.add(fitBarcodeGold);
				}
			}
			if (StringUtils.isNotBlank(fitBarcodeExtra))
			{
				var bestaandeFITExtra = fitService.getFit(fitBarcodeExtra).orElse(null);
				if (bestaandeFITExtra != null && IFOBTType.STUDIE.equals(bestaandeFITExtra.getType())
					&& !bestaandeFITExtra.getColonUitnodigingExtra().equals(uitnodiging))
				{
					addFout(foutmeldingen,
						String.format(KoppelConstants.COLON_BUISID_AL_GEKOPPELD, koppelRegel.getID(),
							StringUtils.defaultIfBlank(fitBarcodeExtra, "<geen>"),
							StringUtils.defaultIfBlank(fitBarcodeExtra, "<geen>"), trackTraceId, "Extra ",
							bestaandeFITExtra.getColonUitnodiging().getUitnodigingsId()));
				}
				if (uitnodiging.getGekoppeldeExtraTest() != null && !uitnodiging.getGekoppeldeExtraTest().getBarcode().equals(fitBarcodeExtra))
				{
					addFout(foutmeldingen,
						String.format(KoppelConstants.COLON_UITNODIGINGSID_AL_GEKOPPELD, koppelRegel.getID(), fitBarcodeGold,
							StringUtils.defaultIfBlank(fitBarcodeExtra, "<geen>"), trackTraceId, "Extra ", uitnodiging.getGekoppeldeExtraTest().getBarcode()));
				}
				var ifobtVervaldatum = houdbaarheidDao.getHoudbaarheidVoor(IFOBTVervaldatum.class, fitBarcodeExtra);
				if (ifobtVervaldatum == null)
				{
					addFout(foutmeldingen, String.format(KoppelConstants.COLON_HOUDBAARHEID_ONBEKEND, koppelRegel.getID(), fitBarcodeGold,
						StringUtils.defaultIfBlank(fitBarcodeExtra, "<geen>"), trackTraceId, "Extra "));
				}
				else if (DateUtil.toLocalDate(ifobtVervaldatum.getVervalDatum()).isBefore(minstensHoudbaarTotMetColon))
				{
					addFout(foutmeldingen, String.format(KoppelConstants.COLON_HOUDBAARHEID_TE_KORT, koppelRegel.getID(), fitBarcodeGold,
						StringUtils.defaultIfBlank(fitBarcodeExtra, "<geen>"), trackTraceId, "Extra "));
				}
				if (barcodeAlTeruggekoppeld(barcodesUitKoppeldata, fitBarcodeExtra))
				{
					addFout(foutmeldingen,
						String.format(KoppelConstants.COLON_BUISID_IS_DUBBEL, koppelRegel.getID(), fitBarcodeGold, fitBarcodeExtra, trackTraceId));
				}
				else
				{
					barcodesUitKoppeldata.add(fitBarcodeExtra);
				}
			}
		}
	}

	private ColonOnderzoeksVariant getOnderzoeksVariant(ColonUitnodiging uitnodiging)
	{
		ColonOnderzoeksVariant onderzoeksVariant = null;
		if (uitnodiging != null)
		{
			onderzoeksVariant = uitnodiging.getOnderzoeksVariant();
		}
		return onderzoeksVariant;
	}

	private ColonUitnodiging getUitnodiging(KOPPELDATA.VERZONDENUITNODIGING koppelRegel)
	{
		var parameters = new HashMap<String, Long>();
		parameters.put("uitnodigingsId", koppelRegel.getID());
		return hibernateService.getUniqueByParameters(ColonUitnodiging.class, parameters);
	}

	@Override
	protected void logKoppelenFout(LogEvent logEvent)
	{
		logService.logGebeurtenis(LogGebeurtenis.IFOBT_CONTROLE_KOPPELEN_FOUT, logEvent, Bevolkingsonderzoek.COLON);
	}

	public void setHoudbaarheidService(BaseHoudbaarheidService houdbaarheidService)
	{
		this.houdbaarheidService = houdbaarheidService;
	}

	public void setFitService(ColonBaseFITService fitService)
	{
		this.fitService = fitService;
	}

	public void setCurrentDateSupplier(ICurrentDateSupplier currentDateSupplier)
	{
		this.currentDateSupplier = currentDateSupplier;
	}

	public void setHoudbaarheidDao(BaseHoudbaarheidDao houdbaarheidDao)
	{
		this.houdbaarheidDao = houdbaarheidDao;
	}

	public void setHibernateService(HibernateService hibernateService)
	{
		this.hibernateService = hibernateService;
	}
}
