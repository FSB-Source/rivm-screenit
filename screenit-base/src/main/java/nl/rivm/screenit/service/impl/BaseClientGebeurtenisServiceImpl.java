package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.text.SimpleDateFormat;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientGebeurtenis;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.ScannedAntwoordFormulier;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.ClientGebeurtenisType;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitstel;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelReden;
import nl.rivm.screenit.service.BaseClientGebeurtenisService;
import nl.rivm.screenit.service.BaseDossierAuditService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.util.BezwaarUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.IFOBTTestUtil;

import org.hibernate.envers.query.AuditEntity;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.support.PropertyComparator;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class BaseClientGebeurtenisServiceImpl implements BaseClientGebeurtenisService
{

	@Autowired
	private MammaBaseStandplaatsService standplaatsService;

	@Autowired
	private BaseDossierAuditService dossierAuditService;

	@Override
	public List<ClientGebeurtenis> getClientColonGebeurtenissen(Client client)
	{
		List<ClientGebeurtenis> gebeurtenissen = new ArrayList<>();

		ColonDossier colonDossier = client.getColonDossier();
		ColonScreeningRonde colonScreeningRonde = colonDossier.getLaatsteScreeningRonde();
		if (colonScreeningRonde != null)
		{
			for (ColonIntakeAfspraak afspraak : colonScreeningRonde.getAfspraken())
			{
				ClientGebeurtenis gebeurtenis = new ClientGebeurtenis();
				gebeurtenis.setDatum(afspraak.getDatumLaatsteWijziging());
				gebeurtenis.setType(ClientGebeurtenisType.INTAKE_AFSPRAAK_GEMAAKT);

				SimpleDateFormat tijdFormat = new SimpleDateFormat("HH:mm");
				gebeurtenis.setExtraParam(DateUtil.toLocalDate(afspraak.getStartTime()).format(DateUtil.LOCAL_DATE_WEERGAVE_CLIENTPORTAAL_FORMAT),
					tijdFormat.format(afspraak.getStartTime()));
				gebeurtenissen.add(gebeurtenis);

				if (AfspraakStatus.VERPLAATST.equals(afspraak.getStatus()))
				{
					gebeurtenis = new ClientGebeurtenis();
					gebeurtenis.setDatum(getVerplaatstDatum(afspraak, colonScreeningRonde.getAfspraken()));
					gebeurtenis.setType(ClientGebeurtenisType.INTAKE_AFSPRAAK_GEWIJZIGD);
					gebeurtenissen.add(gebeurtenis);
				}

				if (AfspraakStatus.isGeannuleerd(afspraak.getStatus()))
				{
					gebeurtenis = new ClientGebeurtenis();
					gebeurtenis.setDatum(afspraak.getAfzegDatum());
					gebeurtenis.setType(ClientGebeurtenisType.INTAKE_AFSPRAAK_AFGEZEGD);
					gebeurtenissen.add(gebeurtenis);
				}

				ColonConclusie conclusie = afspraak.getConclusie();
				if (conclusie != null)
				{
					if (ColonConclusieType.NO_SHOW.equals(conclusie.getType()))
					{
						gebeurtenis = new ClientGebeurtenis();
						gebeurtenis.setDatum(conclusie.getDatum());
						gebeurtenis.setType(ClientGebeurtenisType.INTAKE_AFSPRAAK_NO_SHOW);
						gebeurtenissen.add(gebeurtenis);
					}
				}
			}
			for (ColonUitnodiging colonUitnodiging : colonScreeningRonde.getUitnodigingen())
			{
				if (colonUitnodiging.getDatumTerugOntvangen() != null)
				{
					if (colonUitnodiging.getAntwoordFormulier() != null
						&& !ScannedAntwoordFormulier.STATUS_VERWIJDERD.equals(colonUitnodiging.getAntwoordFormulier().getStatus())
						&& !ScannedAntwoordFormulier.STATUS_VERWIJDERD_UIT_DOSSIER.equals(colonUitnodiging.getAntwoordFormulier().getStatus()))
					{
						ClientGebeurtenis gebeurtenis = new ClientGebeurtenis();
						gebeurtenis.setType(ClientGebeurtenisType.ANTWOORDFORMULIER_ONTVANGEN);
						gebeurtenis.setDatum(colonUitnodiging.getDatumTerugOntvangen());
						gebeurtenissen.add(gebeurtenis);
					}

					Date lastActionOfClient = dossierAuditService.getLastRevisionDate(colonUitnodiging, AuditEntity.property("datumTerugOntvangen").isNotNull(), Client.class);

					if (lastActionOfClient != null)
					{
						ClientGebeurtenis gebeurtenis = new ClientGebeurtenis();
						gebeurtenis.setType(ClientGebeurtenisType.AFNAMEDATUM_INGEVULD);
						gebeurtenis.setDatum(lastActionOfClient);
						gebeurtenissen.add(gebeurtenis);
					}
				}
				IFOBTTest buis = IFOBTTestUtil.getIfobtTest(colonUitnodiging);
				if (buis != null && IFOBTTestStatus.VERLOREN.equals(buis.getStatus()) && colonUitnodiging.getRetourzendingReden() == null)
				{
					ClientGebeurtenis gebeurtenis = new ClientGebeurtenis();
					gebeurtenis.setType(ClientGebeurtenisType.IFOBT_VERLOREN);
					gebeurtenis.setDatum(buis.getStatusDatum());
					gebeurtenissen.add(gebeurtenis);
				}
			}

			for (ColonBrief brief : colonScreeningRonde.getBrieven())
			{
				BriefType briefType = brief.getBriefType();
				switch (briefType)
				{
				case COLON_GUNSTIGE_UITSLAG:
				case COLON_UITNODIGING_INTAKE:

					ClientGebeurtenis gebeurtenis = new ClientGebeurtenis();
					gebeurtenis.setType(ClientGebeurtenisType.UITSLAG_GECOMMUNICEERD);
					if (brief.isGegenereerd())
					{
						MergedBrieven<?> mergedBrieven = brief.getMergedBrieven();
						if (mergedBrieven.getPrintDatum() != null)
						{
							gebeurtenis.setDatum(mergedBrieven.getPrintDatum());
						}
						else
						{
							gebeurtenis.setDatum(mergedBrieven.getCreatieDatum());
						}
					}
					else
					{
						gebeurtenis.setDatum(brief.getCreatieDatum());
					}

					gebeurtenissen.add(gebeurtenis);
					break;
				default:
					break;
				}
			}

			clientEenmaligeAfmeldingGebeurtenissen(gebeurtenissen, colonScreeningRonde);
		}
		clientDefinitiefAfmeldingGebeurtenissen(gebeurtenissen, colonDossier);
		clientBezwaarGebeurtenissen(client, gebeurtenissen, Bevolkingsonderzoek.COLON);

		return gebeurtenissen;
	}

	@Override
	public List<ClientGebeurtenis> getClientCervixGebeurtenissen(Client client)
	{
		List<ClientGebeurtenis> gebeurtenissen = new ArrayList<>();

		CervixDossier dossier = client.getCervixDossier();
		CervixScreeningRonde screeningRonde = dossier.getLaatsteScreeningRonde();
		if (screeningRonde != null)
		{
			for (CervixUitnodiging uitnodiging : screeningRonde.getUitnodigingen())
			{
				if (uitnodiging.getMonsterType() == CervixMonsterType.ZAS && uitnodiging.getGeannuleerdDatum() == null)
				{
					ClientGebeurtenis gebeurtenis = new ClientGebeurtenis();
					gebeurtenis.setType(ClientGebeurtenisType.CERVIX_ZAS_AANGEVRAAGD);
					gebeurtenis.setDatum(uitnodiging.getCreatieDatum());
					gebeurtenis.setExtraParam(DateUtil.toLocalDate(uitnodiging.getUitnodigingsDatum()).format(DateUtil.LOCAL_DATE_WEERGAVE_CLIENTPORTAAL_FORMAT));
					gebeurtenissen.add(gebeurtenis);
				}
			}

			clientEenmaligeAfmeldingGebeurtenissen(gebeurtenissen, screeningRonde);

			for (CervixBrief brief : screeningRonde.getBrieven())
			{
				if (brief.getProjectBrief() == null && brief.getHerdruk() != null && brief.getUitnodiging() != null
					&& Boolean.TRUE.equals(brief.getUitnodiging().getAangevraagdeHerdruk()))
				{
					ClientGebeurtenis gebeurtenis = new ClientGebeurtenis();
					gebeurtenis.setDatum(brief.getCreatieDatum());
					gebeurtenis.setType(ClientGebeurtenisType.CERVIX_HERDRUK);
					gebeurtenissen.add(gebeurtenis);
				}
			}

			CervixMonster monsterHpvUitslag = screeningRonde.getMonsterHpvUitslag();
			CervixUitstrijkje uitstrijkjeCytologieUitslag = screeningRonde.getUitstrijkjeCytologieUitslag();
			CervixUitstrijkje uitstrijkjeVervolgonderzoekUitslag = screeningRonde.getUitstrijkjeVervolgonderzoekUitslag();

			uitslagClientGebeurtenissen(gebeurtenissen, monsterHpvUitslag);
			if (monsterHpvUitslag != null && !monsterHpvUitslag.equals(uitstrijkjeCytologieUitslag))
			{
				uitslagClientGebeurtenissen(gebeurtenissen, uitstrijkjeCytologieUitslag);
			}
			uitslagClientGebeurtenissen(gebeurtenissen, uitstrijkjeVervolgonderzoekUitslag);

			CervixUitstel uitstel = screeningRonde.getUitstel();
			if (uitstel != null)
			{
				ClientGebeurtenis gebeurtenis = new ClientGebeurtenis();
				gebeurtenis.setDatum(uitstel.getWijzigingsDatum());
				gebeurtenis.setType(ClientGebeurtenisType.CERVIX_UITSTEL);
				gebeurtenis.setExtraParam(DateUtil.toLocalDate(uitstel.getUitstellenTotDatum()).format(DateUtil.LOCAL_DATE_WEERGAVE_CLIENTPORTAAL_FORMAT));
				gebeurtenissen.add(gebeurtenis);
			}
		}
		clientDefinitiefAfmeldingGebeurtenissen(gebeurtenissen, dossier);
		clientBezwaarGebeurtenissen(client, gebeurtenissen, Bevolkingsonderzoek.CERVIX);

		return gebeurtenissen;
	}

	private void uitslagClientGebeurtenissen(List<ClientGebeurtenis> gebeurtenissen, CervixMonster monster)
	{
		if (monster != null && monster.getBrief() != null && monster.getBrief().isGegenereerd() && monster.getBrief().getMergedBrieven() != null
			&& Boolean.TRUE.equals(monster.getBrief().getMergedBrieven().getGeprint()))
		{
			ClientGebeurtenis gebeurtenis = new ClientGebeurtenis();
			gebeurtenis.setDatum(monster.getBrief().getMergedBrieven().getPrintDatum());
			gebeurtenis.setType(ClientGebeurtenisType.CERVIX_UITSLAG_GECOMMUNICEERD);
			gebeurtenissen.add(gebeurtenis);
		}
	}

	@Override
	public List<ClientGebeurtenis> getClientMammaGebeurtenissen(Client client)
	{
		List<ClientGebeurtenis> gebeurtenissen = new ArrayList<>();

		MammaDossier dossier = client.getMammaDossier();
		if (dossier != null)
		{
			MammaScreeningRonde screeningRonde = dossier.getLaatsteScreeningRonde();
			if (screeningRonde != null)
			{
				addUitnodigingen(gebeurtenissen, screeningRonde);

				addUitstellen(gebeurtenissen, screeningRonde);

				addBrieven(gebeurtenissen, screeningRonde);

				clientEenmaligeAfmeldingGebeurtenissen(gebeurtenissen, screeningRonde);
			}
			clientDefinitiefAfmeldingGebeurtenissen(gebeurtenissen, dossier);
		}
		clientBezwaarGebeurtenissen(client, gebeurtenissen, Bevolkingsonderzoek.MAMMA);

		return gebeurtenissen;
	}

	private void addUitnodigingen(List<ClientGebeurtenis> gebeurtenissen, MammaScreeningRonde screeningRonde)
	{
		for (MammaUitnodiging uitnodiging : screeningRonde.getUitnodigingen())
		{
			for (MammaAfspraak afspraak : uitnodiging.getAfspraken())
			{
				ClientGebeurtenis gebeurtenis = new ClientGebeurtenis();
				gebeurtenis.setDatum(afspraak.getCreatiedatum());
				gebeurtenis.setType(ClientGebeurtenisType.MAMMA_AFSPRAAK_GEMAAKT);

				SimpleDateFormat tijdFormat = new SimpleDateFormat("HH:mm");
				MammaStandplaatsLocatie locatie = standplaatsService.getStandplaatsLocatie(afspraak.getStandplaatsPeriode().getStandplaatsRonde().getStandplaats(),
					afspraak.getVanaf());
				gebeurtenis.setExtraParam(DateUtil.toLocalDate(afspraak.getVanaf()).format(DateUtil.LOCAL_DATE_WEERGAVE_CLIENTPORTAAL_FORMAT),
					tijdFormat.format(afspraak.getVanaf()), locatie.getPlaats());
				gebeurtenissen.add(gebeurtenis);

				if (MammaAfspraakStatus.VERPLAATST.equals(afspraak.getStatus()))
				{
					gebeurtenis = new ClientGebeurtenis();
					gebeurtenis.setDatum(getVerplaatstDatum(afspraak, uitnodiging.getAfspraken()));
					gebeurtenis.setType(ClientGebeurtenisType.MAMMA_AFSPRAAK_VERPLAATST);
					gebeurtenissen.add(gebeurtenis);
				}
				else if (MammaAfspraakStatus.isGeannuleerd(afspraak.getStatus()) && afspraak.getAfgezegdOp() != null)
				{
					gebeurtenis = new ClientGebeurtenis();
					gebeurtenis.setDatum(afspraak.getAfgezegdOp());
					gebeurtenis.setType(ClientGebeurtenisType.MAMMA_AFSPRAAK_GEANNULEERD);
					gebeurtenissen.add(gebeurtenis);
				}
			}
		}
	}

	private void addUitstellen(List<ClientGebeurtenis> gebeurtenissen, MammaScreeningRonde screeningRonde)
	{
		for (MammaUitstel uitstel : screeningRonde.getUitstellen())
		{
			if (uitstel.getUitstelReden().equals(MammaUitstelReden.CLIENT_CONTACT))
			{
				ClientGebeurtenis gebeurtenis = new ClientGebeurtenis();
				gebeurtenis.setDatum(uitstel.getGemaaktOp());
				gebeurtenis.setType(ClientGebeurtenisType.MAMMA_UITSTEL);

				MammaStandplaatsLocatie locatie = standplaatsService.getStandplaatsLocatie(uitstel.getStandplaats(), uitstel.getStreefDatum());
				gebeurtenis.setExtraParam(DateUtil.toLocalDate(uitstel.getStreefDatum()).format(DateUtil.LOCAL_DATE_WEERGAVE_CLIENTPORTAAL_FORMAT), locatie.getPlaats());
				gebeurtenissen.add(gebeurtenis);
			}
		}
	}

	private void addBrieven(List<ClientGebeurtenis> gebeurtenissen, MammaScreeningRonde screeningRonde)
	{
		for (MammaBrief brief : screeningRonde.getBrieven())
		{
			if (brief.isGegenereerd())
			{
				BriefType briefType = brief.getBriefType();
				ClientGebeurtenisType gebeurtenisType = null;
				if (BriefType.isMammaUitslagBrief(briefType))
				{
					gebeurtenisType = ClientGebeurtenisType.UITSLAG_GECOMMUNICEERD;
				}
				else if (briefType == BriefType.MAMMA_OPEN_UITNODIGING)
				{
					gebeurtenisType = ClientGebeurtenisType.MAMMA_OPEN_UITNODIGING;
				}

				if (gebeurtenisType != null)
				{
					MergedBrieven<?> mergedBrieven = brief.getMergedBrieven();
					Date printDatum = null;
					if (mergedBrieven != null)
					{
						if (mergedBrieven.getPrintDatum() != null)
						{
							printDatum = mergedBrieven.getPrintDatum();
						}
					}
					else
					{
						printDatum = brief.getCreatieDatum();
					}
					if (printDatum != null)
					{
						ClientGebeurtenis gebeurtenis = new ClientGebeurtenis();
						gebeurtenis.setType(gebeurtenisType);
						gebeurtenis.setDatum(printDatum);
						gebeurtenissen.add(gebeurtenis);
					}
				}
			}
		}
	}

	private <A extends Afmelding<?, ?, ?>, S extends ScreeningRonde<?, ?, A, ?>> void clientEenmaligeAfmeldingGebeurtenissen(List<ClientGebeurtenis> gebeurtenissen,
		S screeningRonde)
	{
		for (A afmelding : screeningRonde.getAfmeldingen())
		{
			if (!Boolean.TRUE.equals(afmelding.getImplicieteAfmelding()))
			{
				ClientGebeurtenis gebeurtenis = new ClientGebeurtenis();
				gebeurtenis.setType(ClientGebeurtenisType.EENMALIGE_AFMELDING);
				gebeurtenis.setDatum(afmelding.getAfmeldDatum());
				gebeurtenissen.add(gebeurtenis);
			}
			if (afmelding.getHeraanmeldDatum() != null && !Boolean.TRUE.equals(afmelding.getImplicieteHeraanmelding()))
			{
				ClientGebeurtenis gebeurtenis = new ClientGebeurtenis();
				gebeurtenis.setType(ClientGebeurtenisType.HERAANMELDING);
				gebeurtenis.setDatum(afmelding.getHeraanmeldDatum());
				gebeurtenissen.add(gebeurtenis);
			}
		}
	}

	private <A extends Afmelding<?, ?, ?>, D extends Dossier<?, A>> void clientDefinitiefAfmeldingGebeurtenissen(List<ClientGebeurtenis> gebeurtenissen, D dossier)
	{
		for (A afmelding : dossier.getAfmeldingen())
		{
			if (AanvraagBriefStatus.VERWERKT == afmelding.getAfmeldingStatus())
			{
				ClientGebeurtenis gebeurtenis = new ClientGebeurtenis();
				gebeurtenis.setType(ClientGebeurtenisType.DEFINITIEVE_AFMELDING);
				gebeurtenis.setDatum(afmelding.getAfmeldDatum());
				gebeurtenissen.add(gebeurtenis);
			}

			if (AanvraagBriefStatus.VERWERKT == afmelding.getHeraanmeldStatus())
			{
				ClientGebeurtenis gebeurtenis = new ClientGebeurtenis();
				gebeurtenis.setType(ClientGebeurtenisType.HERAANMELDING);
				gebeurtenis.setDatum(afmelding.getHeraanmeldDatum());
				gebeurtenissen.add(gebeurtenis);
			}
		}
	}

	private void clientBezwaarGebeurtenissen(Client client, List<ClientGebeurtenis> gebeurtenissen, Bevolkingsonderzoek bvo)
	{
		List<BezwaarMoment> bezwaarMomenten = new ArrayList<>(client.getBezwaarMomenten());
		Collections.sort(bezwaarMomenten, new PropertyComparator<>("statusDatum", false, true));
		Map<String, Boolean> bezwaarChanges = new HashMap<>();
		for (BezwaarMoment bezwaarMoment : bezwaarMomenten)
		{
			if (AanvraagBriefStatus.VERWERKT.equals(bezwaarMoment.getStatus()))
			{
				addBezwaarGebeurtenis(gebeurtenissen, bezwaarMoment, bezwaarChanges, BezwaarType.GEEN_KWALITEITSWAARBORGING,
					ClientGebeurtenisType.BEZWAAR_GEMAAKT_KWALITEITSBORGING, ClientGebeurtenisType.BEZWAAR_INGETROKKEN_KWALITEITSBORGING, bvo);
				addBezwaarGebeurtenis(gebeurtenissen, bezwaarMoment, bezwaarChanges, BezwaarType.GEEN_REGISTRATIE_GEBOORTELAND, ClientGebeurtenisType.BEZWAAR_GEMAAKT_GEBOORTELAND,
					ClientGebeurtenisType.BEZWAAR_INGETROKKEN_GEBOORTELAND, null);
				addBezwaarGebeurtenis(gebeurtenissen, bezwaarMoment, bezwaarChanges, BezwaarType.GEEN_UITWISSELING_MET_DE_HUISARTS,
					ClientGebeurtenisType.BEZWAAR_GEMAAKT_COMMUNICATIEHUISARTS, ClientGebeurtenisType.BEZWAAR_INGETROKKEN_COMMUNICATIEHUISARTS, bvo);
				addBezwaarGebeurtenis(gebeurtenissen, bezwaarMoment, bezwaarChanges, BezwaarType.GEEN_WETENSCHAPPELIJK_ONDERZOEK,
					ClientGebeurtenisType.BEZWAAR_GEMAAKT_WETENSCHAPPELIJKONDERZOEK, ClientGebeurtenisType.BEZWAAR_INGETROKKEN_WETENSCHAPPELIJKONDERZOEK, bvo);
				addBezwaarGebeurtenis(gebeurtenissen, bezwaarMoment, bezwaarChanges, BezwaarType.GEEN_GEBRUIK_LICHAAMSMATERIAAL_WETENSCHAPPELIJK_ONDERZOEK,
					ClientGebeurtenisType.BEZWAAR_GEMAAKT_WETENSCHAPPELIJKONDERZOEK_LICHAAMSMATERIAAL,
					ClientGebeurtenisType.BEZWAAR_INGETROKKEN_WETENSCHAPPELIJKONDERZOEK_LICHAAMSMATERIAAL, bvo);
				addBezwaarGebeurtenis(gebeurtenissen, bezwaarMoment, bezwaarChanges, BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER,
					ClientGebeurtenisType.BEZWAAR_GEMAAKT_DOSSIERVOERING, ClientGebeurtenisType.BEZWAAR_INGETROKKEN_DOSSIERVOERING, bvo);
				addBezwaarGebeurtenis(gebeurtenissen, bezwaarMoment, bezwaarChanges, BezwaarType.GEEN_DIGITALE_UITWISSELING_MET_HET_ZIEKENHUIS,
					ClientGebeurtenisType.BEZWAAR_GEMAAKT_DIGITALE_UITWISSELING_MET_HET_ZIEKENHUIS,
					ClientGebeurtenisType.BEZWAAR_INGETROKKEN_DIGITALE_UITWISSELING_MET_HET_ZIEKENHUIS, bvo);
				addBezwaarGebeurtenis(gebeurtenissen, bezwaarMoment, bezwaarChanges, BezwaarType.GEEN_SIGNALERING_VERWIJSADVIES,
					ClientGebeurtenisType.BEZWAAR_GEMAAKT_GEEN_SIGNALERING_VERWIJSADVIES, ClientGebeurtenisType.BEZWAAR_INGETROKKEN_GEEN_SIGNALERING_VERWIJSADVIES, bvo);

			}
		}
	}

	private void addBezwaarGebeurtenis(List<ClientGebeurtenis> gebeurtenissen, BezwaarMoment bezwaarMoment, Map<String, Boolean> bezwaarChanges, BezwaarType type,
		ClientGebeurtenisType gebeurtenisGemaakt, ClientGebeurtenisType gebeurtenisIngetrokken, Bevolkingsonderzoek onderzoek)
	{

		ClientGebeurtenis gebeurtenis = new ClientGebeurtenis();
		gebeurtenis.setDatum(bezwaarMoment.getStatusDatum());
		Boolean value = BezwaarUtil.isBezwaarActiefVoor(bezwaarMoment, type, onderzoek, true);
		Boolean oldValue = bezwaarChanges.get(type.toString());
		if (Boolean.TRUE.equals(value))
		{
			gebeurtenis.setType(gebeurtenisGemaakt);

			if (oldValue == null || !value.equals(oldValue))
			{
				bezwaarChanges.put(type.toString(), value);
				gebeurtenissen.add(gebeurtenis);
			}
		}
		else
		{
			gebeurtenis.setType(gebeurtenisIngetrokken);
			if (oldValue != null && !value.equals(oldValue))
			{
				bezwaarChanges.put(type.toString(), value);
				gebeurtenissen.add(gebeurtenis);
			}
		}
	}

	private Date getVerplaatstDatum(ColonIntakeAfspraak verplaatstAfspraak, List<ColonIntakeAfspraak> afspraken)
	{
		Long maxIdAfstand = null;
		Date verplaatsDatum = null;

		for (ColonIntakeAfspraak afspraak : afspraken)
		{
			long afstandTussenIds = afspraak.getId() - verplaatstAfspraak.getId();
			if (afstandTussenIds > 0L)
			{
				if (maxIdAfstand == null || maxIdAfstand > afstandTussenIds)
				{
					verplaatsDatum = DateUtil.minusTijdseenheid(afspraak.getDatumLaatsteWijziging(), 150, ChronoUnit.MILLIS);
					maxIdAfstand = afstandTussenIds;
				}
			}
		}
		return verplaatsDatum;
	}

	private Date getVerplaatstDatum(MammaAfspraak verplaatstAfspraak, List<MammaAfspraak> afspraken)
	{
		Long huidigeCreatiedatumVerschil = null;
		Date verplaatsDatum = null;

		for (MammaAfspraak afspraak : afspraken)
		{
			long afstandTussenDatums = afspraak.getCreatiedatum().getTime() - verplaatstAfspraak.getCreatiedatum().getTime();
			if (afstandTussenDatums > 0L)
			{
				if (huidigeCreatiedatumVerschil == null || huidigeCreatiedatumVerschil > afstandTussenDatums)
				{
					verplaatsDatum = DateUtil.minusTijdseenheid(afspraak.getCreatiedatum(), 150, ChronoUnit.MILLIS);
					huidigeCreatiedatumVerschil = afstandTussenDatums;
				}
			}
		}
		return verplaatsDatum;
	}
}
