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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import nl.rivm.screenit.KoppelConstants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.service.BarcodeValiderenService;
import nl.rivm.screenit.dao.BaseHoudbaarheidDao;
import nl.rivm.screenit.dao.colon.IFobtDao;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.IFOBTVervaldatum;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.BaseHoudbaarheidService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
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
	private IFobtDao iFobtDao;

	@Autowired
	private BaseHoudbaarheidDao houdbaarheidDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public List<String> voerSemantischeValiatieUit(List<KOPPELDATA.VERZONDENUITNODIGING> koppeldata)
	{
		List<String> foutmeldingen = new ArrayList<>();
		List<String> barcodesUitKoppeldata = new ArrayList<>();

		var vandaag = currentDateSupplier.getLocalDate();
		var minstensHoudbaarTotMetColon = houdbaarheidService.getMinstensHoudbaarTotMet(vandaag, PreferenceKey.PERIODE_MINIMALE_HOUDBAARHEID_IFOBT_MONSTERS_VOOR_CONTROLE);

		for (KOPPELDATA.VERZONDENUITNODIGING verzondenUitnodiging : koppeldata)
		{

			ColonUitnodiging colonUitnodiging = getColonUitnodiging(verzondenUitnodiging);
			ColonOnderzoeksVariant onderzoeksVariant = getOnderzoeksVariant(colonUitnodiging);

			boolean barcodeGoldVerplicht = ColonOnderzoeksVariant.isOfType(onderzoeksVariant, IFOBTType.GOLD);
			boolean barcodeExtraVerplicht = ColonOnderzoeksVariant.isOfType(onderzoeksVariant, IFOBTType.STUDIE);

			String ifobtBarcodeGold = getMatchingFieldValue(verzondenUitnodiging, KoppelConstants.COLON_KOPPEL_BARCODE_GOLD);
			String ifobtBarcodeExtra = getMatchingFieldValue(verzondenUitnodiging, KoppelConstants.COLON_KOPPEL_BARCODE_EXTRA);
			String trackTraceId = getMatchingFieldValue(verzondenUitnodiging, KoppelConstants.KOPPEL_TRACK_ID);

			if (colonUitnodiging == null)
			{
				addFout(foutmeldingen, String.format(KoppelConstants.COLON_UITNODIGINGSID_ONBEKEND, verzondenUitnodiging.getID(), ifobtBarcodeGold,
					StringUtils.defaultIfBlank(ifobtBarcodeExtra, "<geen>"), trackTraceId));
			}
			else
			{
				if (barcodeGoldVerplicht && StringUtils.isBlank(ifobtBarcodeGold))
				{
					addFout(foutmeldingen, String.format(KoppelConstants.COLON_BUISID_MIST_BIJ_TYPE_UITNODIGING, verzondenUitnodiging.getID(),
						StringUtils.defaultIfBlank(ifobtBarcodeGold, "<geen>"), StringUtils.defaultIfBlank(ifobtBarcodeExtra, "<geen>"), trackTraceId, ""));
				}
				if (barcodeExtraVerplicht && StringUtils.isBlank(ifobtBarcodeExtra))
				{
					addFout(foutmeldingen, String.format(KoppelConstants.COLON_BUISID_MIST_BIJ_TYPE_UITNODIGING, verzondenUitnodiging.getID(),
						StringUtils.defaultIfBlank(ifobtBarcodeGold, "<geen>"), StringUtils.defaultIfBlank(ifobtBarcodeExtra, "<geen>"), trackTraceId, "Extra"));
				}
				if (!barcodeGoldVerplicht && StringUtils.isNotBlank(ifobtBarcodeGold))
				{
					addFout(foutmeldingen, String.format(KoppelConstants.COLON_BUISID_ONVERWACHT_TYPE_UITNODIGING, verzondenUitnodiging.getID(),
						StringUtils.defaultIfBlank(ifobtBarcodeGold, "<geen>"), StringUtils.defaultIfBlank(ifobtBarcodeExtra, "<geen>"), trackTraceId, ""));
				}
				if (!barcodeExtraVerplicht && StringUtils.isNotBlank(ifobtBarcodeExtra))
				{
					addFout(foutmeldingen, String.format(KoppelConstants.COLON_BUISID_ONVERWACHT_TYPE_UITNODIGING, verzondenUitnodiging.getID(),
						StringUtils.defaultIfBlank(ifobtBarcodeGold, "<geen>"), StringUtils.defaultIfBlank(ifobtBarcodeExtra, "<geen>"), trackTraceId, "Extra"));
				}
				if (StringUtils.isNotBlank(ifobtBarcodeGold))
				{
					IFOBTTest bestaandeIfobtGoldTest = iFobtDao.getIfobtTest(ifobtBarcodeGold);
					if (bestaandeIfobtGoldTest != null && IFOBTType.GOLD.equals(bestaandeIfobtGoldTest.getType())
						&& !bestaandeIfobtGoldTest.getColonUitnodiging().equals(colonUitnodiging))
					{
						addFout(foutmeldingen, String.format(KoppelConstants.COLON_BUISID_AL_GEKOPPELD, verzondenUitnodiging.getID(),
							StringUtils.defaultIfBlank(ifobtBarcodeGold, "<geen>"),
							StringUtils.defaultIfBlank(ifobtBarcodeExtra, "<geen>"), trackTraceId, "",
							bestaandeIfobtGoldTest.getColonUitnodiging().getUitnodigingsId()));
					}
					if (colonUitnodiging.getGekoppeldeTest() != null && !colonUitnodiging.getGekoppeldeTest().getBarcode().equals(ifobtBarcodeGold))
					{
						addFout(foutmeldingen, String.format(KoppelConstants.COLON_UITNODIGINGSID_AL_GEKOPPELD, verzondenUitnodiging.getID(), ifobtBarcodeGold,
							StringUtils.defaultIfBlank(ifobtBarcodeExtra, "<geen>"), trackTraceId, "", colonUitnodiging.getGekoppeldeTest().getBarcode()));
					}
					IFOBTVervaldatum ifobtVervaldatum = houdbaarheidDao.getHoudbaarheidVoor(IFOBTVervaldatum.class, ifobtBarcodeGold);
					if (ifobtVervaldatum == null)
					{
						addFout(foutmeldingen, String.format(KoppelConstants.COLON_HOUDBAARHEID_ONBEKEND, verzondenUitnodiging.getID(), ifobtBarcodeGold,
							StringUtils.defaultIfBlank(ifobtBarcodeExtra, "<geen>"), trackTraceId, ""));
					}
					else if (DateUtil.toLocalDate(ifobtVervaldatum.getVervalDatum()).isBefore(minstensHoudbaarTotMetColon))
					{
						addFout(foutmeldingen, String.format(KoppelConstants.COLON_HOUDBAARHEID_TE_KORT, verzondenUitnodiging.getID(), ifobtBarcodeGold,
							StringUtils.defaultIfBlank(ifobtBarcodeExtra, "<geen>"), trackTraceId, ""));
					}
					if (barcodeAlTeruggekoppeld(barcodesUitKoppeldata, ifobtBarcodeGold))
					{
						addFout(foutmeldingen, String.format(KoppelConstants.COLON_BUISID_IS_DUBBEL, verzondenUitnodiging.getID(), ifobtBarcodeGold,
							StringUtils.defaultIfBlank(ifobtBarcodeExtra, "<geen>"), trackTraceId));
					}
					else
					{
						barcodesUitKoppeldata.add(ifobtBarcodeGold);
					}
				}
				if (StringUtils.isNotBlank(ifobtBarcodeExtra))
				{
					IFOBTTest bestaandeIfobtExtraTest = iFobtDao.getIfobtTest(ifobtBarcodeExtra);
					if (bestaandeIfobtExtraTest != null && IFOBTType.STUDIE.equals(bestaandeIfobtExtraTest.getType())
						&& !bestaandeIfobtExtraTest.getColonUitnodigingExtra().equals(colonUitnodiging))
					{
						addFout(foutmeldingen,
							String.format(KoppelConstants.COLON_BUISID_AL_GEKOPPELD, verzondenUitnodiging.getID(),
								StringUtils.defaultIfBlank(ifobtBarcodeExtra, "<geen>"),
								StringUtils.defaultIfBlank(ifobtBarcodeExtra, "<geen>"), trackTraceId, "Extra ",
								bestaandeIfobtExtraTest.getColonUitnodiging().getUitnodigingsId()));
					}
					if (colonUitnodiging.getGekoppeldeExtraTest() != null && !colonUitnodiging.getGekoppeldeExtraTest().getBarcode().equals(ifobtBarcodeExtra))
					{
						addFout(foutmeldingen,
							String.format(KoppelConstants.COLON_UITNODIGINGSID_AL_GEKOPPELD, verzondenUitnodiging.getID(), ifobtBarcodeGold,
								StringUtils.defaultIfBlank(ifobtBarcodeExtra, "<geen>"), trackTraceId, "Extra ", colonUitnodiging.getGekoppeldeExtraTest().getBarcode()));
					}
					IFOBTVervaldatum ifobtVervaldatum = houdbaarheidDao.getHoudbaarheidVoor(IFOBTVervaldatum.class, ifobtBarcodeExtra);
					if (ifobtVervaldatum == null)
					{
						addFout(foutmeldingen, String.format(KoppelConstants.COLON_HOUDBAARHEID_ONBEKEND, verzondenUitnodiging.getID(), ifobtBarcodeGold,
							StringUtils.defaultIfBlank(ifobtBarcodeExtra, "<geen>"), trackTraceId, "Extra "));
					}
					else if (DateUtil.toLocalDate(ifobtVervaldatum.getVervalDatum()).isBefore(minstensHoudbaarTotMetColon))
					{
						addFout(foutmeldingen, String.format(KoppelConstants.COLON_HOUDBAARHEID_TE_KORT, verzondenUitnodiging.getID(), ifobtBarcodeGold,
							StringUtils.defaultIfBlank(ifobtBarcodeExtra, "<geen>"), trackTraceId, "Extra "));
					}
					if (barcodeAlTeruggekoppeld(barcodesUitKoppeldata, ifobtBarcodeExtra))
					{
						addFout(foutmeldingen,
							String.format(KoppelConstants.COLON_BUISID_IS_DUBBEL, verzondenUitnodiging.getID(), ifobtBarcodeGold, ifobtBarcodeExtra, trackTraceId));
					}
					else
					{
						barcodesUitKoppeldata.add(ifobtBarcodeExtra);
					}
				}
			}
		}
		return foutmeldingen;
	}

	private ColonOnderzoeksVariant getOnderzoeksVariant(ColonUitnodiging colonUitnodiging)
	{
		ColonOnderzoeksVariant onderzoeksVariant = null;
		if (colonUitnodiging != null)
		{
			onderzoeksVariant = colonUitnodiging.getOnderzoeksVariant();
		}
		return onderzoeksVariant;
	}

	private ColonUitnodiging getColonUitnodiging(KOPPELDATA.VERZONDENUITNODIGING verzondenUitnodiging)
	{
		HashMap<String, Long> parameters = new HashMap<>();
		parameters.put("uitnodigingsId", verzondenUitnodiging.getID());
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

	public void setiFobtDao(IFobtDao iFobtDao)
	{
		this.iFobtDao = iFobtDao;
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
