package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.EnumMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.ClientDao;
import nl.rivm.screenit.dao.cervix.CervixRondeDao;
import nl.rivm.screenit.main.model.AfmeldenDossierGebeurtenis;
import nl.rivm.screenit.main.model.DossierGebeurtenis;
import nl.rivm.screenit.main.model.DossierGebeurtenisType;
import nl.rivm.screenit.main.model.OpenUitnodigingDossierGebeurtenis;
import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenissen;
import nl.rivm.screenit.main.model.TypeGebeurtenis;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaBeoordelingGebeurtenis;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaLezingGebeurtenis;
import nl.rivm.screenit.main.model.mamma.onderzoek.MammaOnderzoekGebeurtenis;
import nl.rivm.screenit.main.service.ClientDossierFilter;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.service.cervix.CervixOmissieGebeurtenisFactory;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.mamma.MammaFollowUpRadiologieVerslagGebeurtenis;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.mamma.MammaUploadBeeldenVerzoekGebeurtenis;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.DigitaalClientBericht;
import nl.rivm.screenit.model.InpakbareUitnodiging;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixCytologieOrder;
import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixHpvBeoordeling;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie;
import nl.rivm.screenit.model.cervix.enums.CervixAfmeldingReden;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieOrderStatus;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsBerichtStatus;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.enums.CervixZasStatus;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonHuisartsBericht;
import nl.rivm.screenit.model.colon.ColonHuisartsBerichtStatus;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.colon.ColonVooraankondiging;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.OpenUitnodiging;
import nl.rivm.screenit.model.colon.ScannedAntwoordFormulier;
import nl.rivm.screenit.model.colon.enums.ColonAfmeldingReden;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.colon.enums.RetourzendingWijze;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.DigitaalBerichtType;
import nl.rivm.screenit.model.enums.GebeurtenisBron;
import nl.rivm.screenit.model.enums.OpenUitnodigingUitslag;
import nl.rivm.screenit.model.envers.ScreenitRevisionEntity;
import nl.rivm.screenit.model.mamma.MammaAfmelding;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitstel;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenPoging;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenVerzoek;
import nl.rivm.screenit.model.mamma.berichten.MammaHuisartsBericht;
import nl.rivm.screenit.model.mamma.enums.MammaAfmeldingReden;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.MammaZijde;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectBriefActieType;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectVragenlijstAntwoordenHolder;
import nl.rivm.screenit.model.project.ProjectVragenlijstStatus;
import nl.rivm.screenit.model.project.ScannedVragenlijst;
import nl.rivm.screenit.service.BaseDossierAuditService;
import nl.rivm.screenit.service.RondeNummerService;
import nl.rivm.screenit.service.colon.ColonVerwerkVerslagService;
import nl.rivm.screenit.service.mamma.MammaBaseFollowUpService;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.rivm.screenit.util.FITTestUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.collections.CollectionUtils;
import org.hibernate.envers.AuditReader;
import org.hibernate.envers.AuditReaderFactory;
import org.hibernate.envers.RevisionType;
import org.hibernate.envers.query.AuditEntity;
import org.hibernate.envers.query.AuditQuery;
import org.hibernate.envers.query.criteria.AuditCriterion;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.primitives.Ints;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
@AllArgsConstructor
@Slf4j
public class DossierServiceImpl implements DossierService
{

	private final ClientDao clientDao;

	private final HibernateService hibernateService;

	private final CervixRondeDao cervixRondeDao;

	private final RondeNummerService rondeNummerService;

	private final BaseDossierAuditService dossierAuditService;

	private final MammaBaseFollowUpService baseFollowUpService;

	private final ColonVerwerkVerslagService colonVerwerkVerslagService;

	@Override
	public List<ScreeningRondeGebeurtenissen> getScreeningRondeGebeurtenissen(Client client, ClientDossierFilter clientDossierFilter)
	{
		List<ScreeningRondeGebeurtenissen> dossiers = new ArrayList<>();

		if (clientDossierFilter.getBevolkingsonderzoeken().contains(Bevolkingsonderzoek.CERVIX))
		{
			List<ScreeningRondeGebeurtenissen> cervixRondeDossiers = getCervixScreeningRondeGebeurtenissen(client, Boolean.TRUE.equals(clientDossierFilter.getLaatsteRondes()));
			dossiers.addAll(cervixRondeDossiers);
		}

		if (clientDossierFilter.getBevolkingsonderzoeken().contains(Bevolkingsonderzoek.MAMMA))
		{
			dossiers.addAll(getMammaScreeningRondeGebeurtenissen(client, Boolean.TRUE.equals(clientDossierFilter.getLaatsteRondes())));
		}

		if (clientDossierFilter.getBevolkingsonderzoeken().contains(Bevolkingsonderzoek.COLON))
		{
			List<ScreeningRondeGebeurtenissen> colonRondeDossiers = getColonScreeningRondeGebeurtenissen(client, Boolean.TRUE.equals(clientDossierFilter.getLaatsteRondes()));
			if (colonRondeDossiers != null)
			{
				dossiers.addAll(colonRondeDossiers);
			}
		}
		return dossiers;
	}

	private List<ScreeningRondeGebeurtenissen> getColonScreeningRondeGebeurtenissen(Client client, boolean alleenLaatste)
	{
		List<ScreeningRondeGebeurtenissen> dossiers = new ArrayList<>();
		List<ColonScreeningRonde> screeningRondes = new ArrayList<>(client.getColonDossier().getScreeningRondes());
		Collections.sort(screeningRondes, (o1, o2) -> o1.getCreatieDatum().compareTo(o2.getCreatieDatum()) * -1);

		int i = screeningRondes.size();
		for (ColonScreeningRonde colonScreeningRonde : screeningRondes)
		{
			ScreeningRondeGebeurtenissen rondeDossier = new ScreeningRondeGebeurtenissen(i--);
			rondeDossier.setScreeningRonde(colonScreeningRonde);
			hibernateService.reload(colonScreeningRonde);

			if (i == 0)
			{
				if (client.getColonDossier().getColonVooraankondiging() != null)
				{
					ColonVooraankondiging vooraankondiging = client.getColonDossier().getColonVooraankondiging();
					ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
					screeningRondeGebeurtenis.setDatum(vooraankondiging.getCreatieDatum());
					screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.VOORAANKONDIGING);
					screeningRondeGebeurtenis.setExtraOmschrijving(vooraankondiging.getBrief().getTemplateNaam());
					screeningRondeGebeurtenis.setBrief(vooraankondiging.getBrief());
					screeningRondeGebeurtenis.setBron(GebeurtenisBron.AUTOMATISCH);
					rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
				}
			}

			for (ColonIntakeAfspraak afspraak : colonScreeningRonde.getAfspraken())
			{
				ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
				screeningRondeGebeurtenis.setDatum(afspraak.getDatumLaatsteWijziging());
				screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.INTAKEAFSPRAAKGEMAAKT);
				screeningRondeGebeurtenis
					.setBron(bepaalGebeurtenisBron(afspraak, AuditEntity.property("status").in(new AfspraakStatus[] { AfspraakStatus.GEPLAND, AfspraakStatus.UITGEVOERD })));

				String afspraakTime = Constants.getDateTimeFormat().format(afspraak.getStartTime());

				ColoscopieCentrum coloscopieCentrum = afspraak.getLocation().getColoscopieCentrum();

				screeningRondeGebeurtenis.setExtraOmschrijving(afspraakTime, " Intakelocatie: " + coloscopieCentrum.getNaam() + ", " + coloscopieCentrum.getEerstePlaats());

				rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);

				if (AfspraakStatus.isGeannuleerd(afspraak.getStatus()))
				{
					screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
					screeningRondeGebeurtenis.setDatum(afspraak.getAfzegDatum());
					screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.INTAKEAFSPRAAKAFGEZEGD);
					screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(afspraak));
					rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
				}

				conclusieIntakeGebeurtenissen(rondeDossier, afspraak);
			}
			for (ColonUitnodiging colonUitnodiging : colonScreeningRonde.getUitnodigingen())
			{
				ScreeningRondeGebeurtenis uitnodigingAangemaakt = new ScreeningRondeGebeurtenis();
				uitnodigingAangemaakt.setDatum(colonUitnodiging.getCreatieDatum());
				uitnodigingAangemaakt.setGebeurtenis(TypeGebeurtenis.UITNODIGING_AANGEMAAKT);
				uitnodigingAangemaakt.setExtraOmschrijving("klaarzetten voor inpakcentrum op: ", Constants.getDateFormat().format(colonUitnodiging.getUitnodigingsDatum()));
				uitnodigingAangemaakt.setBron(bepaalGebeurtenisBron(colonUitnodiging, false));
				rondeDossier.addGebeurtenis(uitnodigingAangemaakt);

				if (colonUitnodiging.getVerstuurdDatum() != null)
				{
					ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
					screeningRondeGebeurtenis.setDatum(colonUitnodiging.getVerstuurdDatum());
					screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.UITNODIGING);
					screeningRondeGebeurtenis.setUitnodiging(colonUitnodiging);

					screeningRondeGebeurtenis.setExtraOmschrijving(colonUitnodiging.getTemplateNaam());
					screeningRondeGebeurtenis.setBron(GebeurtenisBron.AUTOMATISCH);
					rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
				}

				ScannedAntwoordFormulier antwoordFormulier = colonUitnodiging.getAntwoordFormulier();
				if (antwoordFormulier != null)
				{
					ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
					screeningRondeGebeurtenis.setDatum(antwoordFormulier.getScanDatum());

					screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.ANTWOORDFORMULIERONTVANGEN);
					if (ScannedAntwoordFormulier.STATUS_VERWIJDERD_UIT_DOSSIER.equals(antwoordFormulier.getStatus())
						|| ScannedAntwoordFormulier.STATUS_VERWIJDERD.equals(antwoordFormulier.getStatus()))
					{
						screeningRondeGebeurtenis.setExtraOmschrijving("UitnodigingsID: " + colonUitnodiging.getUitnodigingsId(), "OCR." + antwoordFormulier.getStatus());
					}
					else
					{
						screeningRondeGebeurtenis.setExtraOmschrijving("UitnodigingsID: " + colonUitnodiging.getUitnodigingsId());
					}
					screeningRondeGebeurtenis.setUitnodiging(colonUitnodiging);
					screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(antwoordFormulier));
					rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
				}
				if (colonUitnodiging.getDatumTerugOntvangen() != null)
				{
					Date lastActionOfClient = dossierAuditService.getLastRevisionDate(colonUitnodiging, AuditEntity.property("datumTerugOntvangen").isNotNull(), Client.class);
					if (lastActionOfClient != null)
					{
						ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
						screeningRondeGebeurtenis.setDatum(lastActionOfClient);
						screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.AFNAMEDATUM_INGEVULD_OP_PORTAAL);
						screeningRondeGebeurtenis.setBron(GebeurtenisBron.CLIENT);
						screeningRondeGebeurtenis.setExtraOmschrijving(Constants.getDateFormat().format(getClientAfnameDatum(colonUitnodiging.getGekoppeldeTest())));
						rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
					}
				}
				retouren(rondeDossier, colonUitnodiging);
				IFOBTTest buis = FITTestUtil.getFITTest(colonUitnodiging);

				if (buis != null && buis.getBarcode() != null)
				{
					ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
					screeningRondeGebeurtenis.setDatum(buis.getDatumVerstuurd());
					screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.UITNODIGINGSPAKKET_SAMENGESTELD);
					String extraOmschrijving = contructBarcodeInfo(colonUitnodiging);
					screeningRondeGebeurtenis.setExtraOmschrijving("UitnodigingsID: " + colonUitnodiging.getUitnodigingsId(), extraOmschrijving);
					screeningRondeGebeurtenis.setBron(GebeurtenisBron.AUTOMATISCH);
					rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
				}
			}

			for (ColonVerslag<?> verslag : colonScreeningRonde.getVerslagen())
			{
				Date ontvangen;
				if (verslag.getOntvangenBericht() != null)
				{
					ontvangen = verslag.getOntvangenBericht().getOntvangen();
				}
				else
				{
					ontvangen = verslag.getDatumVerwerkt();
				}
				if (verslag.getType().equals(VerslagType.MDL) && verslag.getStatus() == VerslagStatus.AFGEROND)
				{
					ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
					screeningRondeGebeurtenis.setDatum(ontvangen);
					screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.UITSLAGCOLOSCOPIEONTVANGEN);
					screeningRondeGebeurtenis.setVerslag(verslag);
					screeningRondeGebeurtenis.setExtraOmschrijving(EnumStringUtil.getPropertyString(((MdlVerslag) verslag).getVervolgbeleid()));
					screeningRondeGebeurtenis.setScreeningRondeGebeurtenissen(rondeDossier);
					screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(verslag));
					rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
				}
				else if (verslag.getType().equals(VerslagType.PA_LAB) && verslag.getStatus() == VerslagStatus.AFGEROND)
				{
					ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
					screeningRondeGebeurtenis.setDatum(ontvangen);
					screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.UITSLAGPATHOLOGIEONTVANGEN);
					screeningRondeGebeurtenis.setVerslag(verslag);
					screeningRondeGebeurtenis.setScreeningRondeGebeurtenissen(rondeDossier);
					screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(verslag));
					rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
				}
			}

			if (colonScreeningRonde.getColonHuisarts() != null && colonScreeningRonde.getDatumVastleggenHuisarts() != null)
			{
				rondeDossier.addGebeurtenis(maakHuisartsToegevoegd(colonScreeningRonde, TypeGebeurtenis.COLON_HUISARTS_TOEGEVOEGD,
					colonScreeningRonde.getDatumVastleggenHuisarts()));
			}

			for (IFOBTTest buis : colonScreeningRonde.getIfobtTesten())
			{
				TypeGebeurtenis typeGebeurtenis = null;
				switch (buis.getStatus())
				{
				case VERLOREN:
					typeGebeurtenis = TypeGebeurtenis.IFOBTVERLOREN;
					break;
				case NIETONTVANGEN:
					typeGebeurtenis = TypeGebeurtenis.RETOURPERIODEIFOBTVERSTREKEN;
					break;
				case NIETTEBEOORDELEN:
					typeGebeurtenis = TypeGebeurtenis.IFOBTNIETTEBEOORDELEN;
					break;
				case VERVALDATUMVERLOPEN:
					typeGebeurtenis = TypeGebeurtenis.VERVALDATUMIFOBTVERLOPEN;
					break;
				case UITGEVOERD:
				case WACHTOPBRIEF:
				case VERWIJDERD:
					typeGebeurtenis = TypeGebeurtenis.UITSLAGIFOBTONTVANGEN;
					break;
				case ONBETROUWBAAR:
					typeGebeurtenis = TypeGebeurtenis.UITSLAGIFOBTONBETROUWBAAR;
					break;
				default:
					if (buis.getUitslag() != null)
					{
						typeGebeurtenis = TypeGebeurtenis.UITSLAGIFOBTONTVANGEN;
					}
					break;
				}

				if (typeGebeurtenis != null)
				{
					String barcodeInfo = "Barcode: " + buis.getBarcode();
					ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
					screeningRondeGebeurtenis.setDatum(buis.getStatusDatum());
					screeningRondeGebeurtenis.setGebeurtenis(typeGebeurtenis);
					screeningRondeGebeurtenis.setUitnodiging(FITTestUtil.getUitnodiging(buis));
					screeningRondeGebeurtenis.setBuis(buis);
					screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(buis, false));
					screeningRondeGebeurtenis.setExtraOmschrijving(barcodeInfo);
					if (TypeGebeurtenis.IFOBTVERLOREN.equals(typeGebeurtenis))
					{
						screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(buis, true));
					}
					else if (TypeGebeurtenis.UITSLAGIFOBTONTVANGEN.equals(typeGebeurtenis) && buis.getBarcode() != null)
					{
						if (buis.getVerwerkingsDatum() != null)
						{
							screeningRondeGebeurtenis.setDatum(buis.getVerwerkingsDatum());
						}
						if (FITTestUtil.isOngunstig(buis))
						{
							screeningRondeGebeurtenis.setExtraOmschrijving("Ongunstig", barcodeInfo);
						}
						else if (FITTestUtil.isGunstig(buis))
						{
							screeningRondeGebeurtenis.setExtraOmschrijving("Gunstig", barcodeInfo);
						}
						else if (IFOBTTestStatus.VERWIJDERD.equals(buis.getStatus()))
						{
							screeningRondeGebeurtenis.setExtraOmschrijving("Verwijderd", barcodeInfo);
						}
					}
					else if (TypeGebeurtenis.IFOBTNIETTEBEOORDELEN.equals(typeGebeurtenis))
					{
						screeningRondeGebeurtenis.setExtraOmschrijving(EnumStringUtil.getPropertyString(buis.getRedenNietTeBeoordelen()), barcodeInfo);
					}

					rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
				}

			}

			voegBrievenGebeurtenissenToe(colonScreeningRonde, rondeDossier);

			rondeDossier.addGebeurtenissen(getScreeningRondeAfmeldingHeraanmeldingGebeurtenissen(colonScreeningRonde));
			rondeDossier.addGebeurtenis(getScreeningRondeStatus(colonScreeningRonde));

			if (CollectionUtils.isNotEmpty(colonScreeningRonde.getHuisartsBerichten()))
			{
				for (ColonHuisartsBericht bericht : colonScreeningRonde.getHuisartsBerichten())
				{
					ColonHuisartsBerichtStatus statusBericht = bericht.getStatus();
					ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
					screeningRondeGebeurtenis.setClickable(true);
					screeningRondeGebeurtenis.setExtraOmschrijving(bericht.getBerichtType().getNaam());
					if (bericht.getHuisarts() != null)
					{
						screeningRondeGebeurtenis.addToExtraOmschrijving(bericht.getHuisarts().getPraktijknaam());
					}
					screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(bericht));
					rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
					screeningRondeGebeurtenis.setColonHuisartsBericht(bericht);

					switch (statusBericht)
					{
					case TE_CONTROLEREN:
					case CONTROLE_NIET_NODIG:
						screeningRondeGebeurtenis.setDatum(bericht.getAanmaakDatum());
						screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.COLON_HUISARTSBERICHT_AANGEMAAKT);
						break;
					case GEEN_HUISARTS:
					case VERZENDEN_MISLUKT:
						screeningRondeGebeurtenis.setDatum(bericht.getAanmaakDatum());
						screeningRondeGebeurtenis.setGebeurtenis(
							bericht.isEenOpnieuwVerzondenBericht() ?
								TypeGebeurtenis.COLON_HUISARTSBERICHT_OPNIEUW_VERSTUREN_MISLUKT :
								TypeGebeurtenis.COLON_HUISARTSBERICHT_VERSTUREN_MISLUKT);
						break;
					case VERZENDEN_GELUKT:
						screeningRondeGebeurtenis.setDatum(bericht.getVerzendDatum());
						screeningRondeGebeurtenis.setGebeurtenis(
							bericht.isEenOpnieuwVerzondenBericht() ?
								TypeGebeurtenis.COLON_HUISARTSBERICHT_OPNIEUW_VERSTUURD :
								TypeGebeurtenis.COLON_HUISARTSBERICHT_VERSTUURD);
						break;
					}
				}
			}

			dossiers.add(rondeDossier);
			if (alleenLaatste && i + 1 == screeningRondes.size())
			{
				break;
			}
		}
		return dossiers;
	}

	private void conclusieIntakeGebeurtenissen(ScreeningRondeGebeurtenissen rondeDossier, ColonIntakeAfspraak afspraak)
	{
		ColoscopieCentrum intakelocatie = afspraak.getLocation().getColoscopieCentrum();
		AtomicBoolean showVerwijderd = new AtomicBoolean(true);
		AtomicBoolean first = new AtomicBoolean(true);
		ColonConclusie prevAuditIntakeConclusie = null;
		boolean heeftActueelGeenConclusie = afspraak.getConclusie() == null;
		boolean heeftAuditedAfspraak = false;
		for (Object auditRow : EntityAuditUtil.getEntityHistory(afspraak, hibernateService.getHibernateSession(), false)) 
		{
			heeftAuditedAfspraak = true;
			ColonIntakeAfspraak auditAfspraak = EntityAuditUtil.getRevisionEntity(auditRow);
			ColonConclusie auditIntakeConclusie = auditAfspraak.getConclusie();
			if (prevAuditIntakeConclusie != null && auditIntakeConclusie != null
				&& !HibernateHelper.getId(prevAuditIntakeConclusie).equals(HibernateHelper.getId(auditIntakeConclusie))
				|| prevAuditIntakeConclusie != null && auditIntakeConclusie == null || prevAuditIntakeConclusie == null && auditIntakeConclusie != null)
			{
				if (auditIntakeConclusie != null)
				{
					addConclusieGebeurtenissen(auditIntakeConclusie, rondeDossier, afspraak, intakelocatie, heeftActueelGeenConclusie, showVerwijderd, first);
				}
			}
			prevAuditIntakeConclusie = auditIntakeConclusie;
		}
		if (!heeftAuditedAfspraak && !heeftActueelGeenConclusie)
		{
			addConclusieGebeurtenissen(afspraak.getConclusie(), rondeDossier, afspraak, intakelocatie, heeftActueelGeenConclusie, showVerwijderd, first);
		}
	}

	private void addConclusieGebeurtenissen(ColonConclusie intakeConclusie, ScreeningRondeGebeurtenissen rondeDossier, ColonIntakeAfspraak afspraak,
		ColoscopieCentrum intakelocatie, boolean heeftActueelEenConclusie, AtomicBoolean showVerwijderd, AtomicBoolean first)
	{
		boolean heeftAuditedConclusies = false;
		for (Object auditConclusieRow : EntityAuditUtil.getEntityHistory(intakeConclusie, hibernateService.getHibernateSession(),
			AuditEntity.property("noShowBericht").isNull(), false))
		{
			heeftAuditedConclusies = true;
			RevisionType revType = EntityAuditUtil.getRevisionType(auditConclusieRow);
			if (revType != RevisionType.DEL)
			{
				ColonConclusie auditConclusie = EntityAuditUtil.getRevisionEntity(auditConclusieRow);
				ScreeningRondeGebeurtenis screeningRondeGebeurtenis = addConclusieGebeurtenis(rondeDossier, afspraak, intakelocatie, auditConclusie);
				if (first.get())
				{
					first.set(false);
				}
				else
				{
					screeningRondeGebeurtenis.setClickable(false);
				}
				if (heeftActueelEenConclusie)
				{
					screeningRondeGebeurtenis.setClickable(false);
				}
			}
			else if (showVerwijderd.get())
			{
				ScreenitRevisionEntity screenitRevisionEntity = EntityAuditUtil.getRevisionInfo(auditConclusieRow);
				ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
				screeningRondeGebeurtenis.setDatum(new Date(screenitRevisionEntity.getTimestamp()));
				screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.CONCLUSIEINTAKEVERWIJDERD);
				screeningRondeGebeurtenis.setBron(dossierAuditService.getGebeurtenisBron(screenitRevisionEntity));
				screeningRondeGebeurtenis.setAfspraak(afspraak);
				rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
			}
			showVerwijderd.set(false);
		}
		if (!heeftAuditedConclusies)
		{
			addConclusieGebeurtenis(rondeDossier, afspraak, intakelocatie, afspraak.getConclusie());
		}
	}

	private ScreeningRondeGebeurtenis addConclusieGebeurtenis(ScreeningRondeGebeurtenissen rondeDossier, ColonIntakeAfspraak afspraak, ColoscopieCentrum intakelocatie,
		ColonConclusie conclusie)
	{
		ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
		screeningRondeGebeurtenis.setDatum(conclusie.getDatum());
		screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.CONCLUSIEINTAKEONTVANGEN);
		List<String> extraOmschrijving = new ArrayList<>();
		extraOmschrijving.add("Conclusie: ");
		extraOmschrijving.add(EnumStringUtil.getPropertyString(conclusie.getType()));
		extraOmschrijving.add("Intakelocatie: " + intakelocatie.getNaam());
		extraOmschrijving.add(intakelocatie.getEerstePlaats());
		screeningRondeGebeurtenis.setExtraOmschrijving(extraOmschrijving.toArray(new String[] {}));
		screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(conclusie));
		screeningRondeGebeurtenis.setAfspraak(afspraak);
		rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
		return screeningRondeGebeurtenis;
	}

	private <U extends InpakbareUitnodiging<?>> void retouren(ScreeningRondeGebeurtenissen rondeDossier, U uitnodiging)
	{
		if (uitnodiging.getRetourOntvangen() != null)
		{
			ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
			screeningRondeGebeurtenis.setDatum(uitnodiging.getRetourOntvangen());
			if (RetourzendingWijze.BESTAND.equals(uitnodiging.getRetourzendingWijze()))
			{
				screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.RETOURZENDING_BESTAND);
			}
			else if (RetourzendingWijze.HANDMATIG.equals(uitnodiging.getRetourzendingWijze()))
			{
				screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.RETOURZENDING_HANDMATIG);
			}
			screeningRondeGebeurtenis.setUitnodiging(uitnodiging);
			screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(uitnodiging));
			String extraOmschrijving = "Reden: " + uitnodiging.getRetourzendingReden() + "; ";
			if (uitnodiging.getRetourzendingStatus() != null)
			{
				switch (uitnodiging.getRetourzendingStatus())
				{
				case NIEUWE_GBA_ADRES_AANGEVRAAGD:
					extraOmschrijving += "adres opnieuw opgevraagd";
					break;
				case NIEUWE_UITNODIGING_AANGEVRAAGD:
					extraOmschrijving += "adres opnieuw opgevraagd; poststuk opnieuw aangemaakt";
					break;
				case NIEUWE_UITNODIGING_DIRECT_AANGEVRAAGD:
					extraOmschrijving += "poststuk opnieuw aangemaakt";
					break;
				case GEEN_NIEUWE_UITNODIGING_NODIG:
					extraOmschrijving += "adres opnieuw opgevraagd; poststuk niet meer nodig";
					break;
				case GEEN_NIEUWE_UITNODIGING_MOGELIJK:
					extraOmschrijving += "adres opnieuw opgevraagd; poststuk definitief onbestelbaar";
					break;
				case TIJDELIJK_ADRES_GEEN_NIEUWE_UITNODIGING_MOGELIJK:
					extraOmschrijving += "tijdelijk adres, dus geen nieuwe adres opgevraagd";
					break;
				case NIEUWE_UITNODIGING_AANGEVRAAGD_MET_TIJDELIJK_ADRES:
					extraOmschrijving += "adres opnieuw opgevraagd; poststuk opnieuw aangemaakt met tijdelijk adres";
					break;
				default:
					break;

				}
			}
			screeningRondeGebeurtenis.setExtraOmschrijving(extraOmschrijving);

			rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
		}
	}

	private String contructBarcodeInfo(ColonUitnodiging colonUitnodiging)
	{
		ColonOnderzoeksVariant onderzoeksVariant = colonUitnodiging.getOnderzoeksVariant();
		String alleBarcodes = "Barcode: ";
		if (ColonOnderzoeksVariant.isOfType(onderzoeksVariant, IFOBTType.GOLD))
		{
			alleBarcodes += colonUitnodiging.getGekoppeldeTest().getBarcode();
			if (onderzoeksVariant.equals(ColonOnderzoeksVariant.VERGELIJKEND) || onderzoeksVariant.equals(ColonOnderzoeksVariant.TB_PAIRED))
			{
				alleBarcodes += "(G)";
			}
		}
		if (ColonOnderzoeksVariant.isOfType(onderzoeksVariant, IFOBTType.STUDIE) || ColonOnderzoeksVariant.isOfType(onderzoeksVariant, IFOBTType.EIKEN))
		{
			alleBarcodes += "/" + colonUitnodiging.getGekoppeldeExtraTest().getBarcode();
			alleBarcodes += ColonOnderzoeksVariant.isOfType(onderzoeksVariant, IFOBTType.STUDIE) ? "(S)" : "(E)";
		}

		return alleBarcodes;
	}

	private <A extends Afmelding<?, ?, ?>, S extends ScreeningRonde<?, ?, A, ?>> List<ScreeningRondeGebeurtenis> getScreeningRondeAfmeldingHeraanmeldingGebeurtenissen(
		S screeningRonde)
	{
		List<ScreeningRondeGebeurtenis> screeningRondeGebeurtenissen = new ArrayList<>();

		for (A afmelding : screeningRonde.getAfmeldingen())
		{
			ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();

			if (AfmeldingType.EENMALIG.equals(afmelding.getType()))
			{
				screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.AFMELDING_EENMALIG);
			}
			else
			{
				screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.AFMELDING_TIJDELIJK);
			}
			screeningRondeGebeurtenis.setDatum(afmelding.getAfmeldDatum());
			screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(afmelding, AuditEntity.conjunction().add(AuditEntity.property("heraanmeldDatum").isNull())
				.add(AuditEntity.property("heraanmeldStatus").isNull()).add(AuditEntity.property("statusHeraanmeldDatum").isNull())));
			switch (afmelding.getBevolkingsonderzoek())
			{
			case COLON:
				if (AfmeldingType.TIJDELIJK.equals(afmelding.getType()) && AanvraagBriefStatus.BRIEF.equals(afmelding.getAfmeldingStatus()))
				{
					screeningRondeGebeurtenis.setExtraOmschrijving("AfmeldingReden.AANVRAAG_DEFINITIEVE_TIJDELIJKE_AFMELDING_COLON");
				}
				else if (Boolean.TRUE.equals(afmelding.getImplicieteAfmelding()))
				{
					screeningRondeGebeurtenis.setExtraOmschrijving("AfmeldingReden.AANVRAAG_DEFINITIEVE_TIJDELIJKE_AFMELDING_COLON");
				}
				else
				{
					screeningRondeGebeurtenis.setExtraOmschrijving(ColonAfmeldingReden.class.getSimpleName() + ".null");
				}
				break;
			case CERVIX:
				CervixAfmeldingReden cervixAfmeldingReden = ((CervixAfmelding) afmelding).getReden();
				if (cervixAfmeldingReden != null)
				{
					screeningRondeGebeurtenis.setExtraOmschrijving(EnumStringUtil.getPropertyString(cervixAfmeldingReden));
				}
				else if (Boolean.TRUE.equals(afmelding.getImplicieteAfmelding()))
				{
					screeningRondeGebeurtenis.setExtraOmschrijving("AfmeldingReden.AANVRAAG_DEFINITIEVE_AFMELDING");
				}
				break;
			case MAMMA:
				MammaAfmeldingReden mammaAfmeldingReden = ((MammaAfmelding) afmelding).getReden();
				if (mammaAfmeldingReden != null)
				{
					screeningRondeGebeurtenis.setExtraOmschrijving(EnumStringUtil.getPropertyString(mammaAfmeldingReden));
				}
				else if (Boolean.TRUE.equals(afmelding.getImplicieteAfmelding()))
				{
					screeningRondeGebeurtenis.setExtraOmschrijving("AfmeldingReden.AANVRAAG_DEFINITIEVE_AFMELDING");
				}
				break;
			}

			screeningRondeGebeurtenissen.add(screeningRondeGebeurtenis);
			if (afmelding.getHeraanmeldDatum() != null)
			{
				screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
				screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.HERAANMELDING);
				screeningRondeGebeurtenis.setDatum(afmelding.getHeraanmeldDatum());
				screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(afmelding));
				screeningRondeGebeurtenissen.add(screeningRondeGebeurtenis);
			}
		}
		return screeningRondeGebeurtenissen;
	}

	private ScreeningRondeGebeurtenis getScreeningRondeStatus(ScreeningRonde<?, ?, ?, ?> screeningRonde)
	{
		if (screeningRonde.getStatus() == ScreeningRondeStatus.AFGEROND)
		{
			ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
			screeningRondeGebeurtenis.setDatum(screeningRonde.getStatusDatum());
			screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.AFGEROND);
			screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(screeningRonde, AuditEntity.property("status").eq(ScreeningRondeStatus.AFGEROND), false));
			if (screeningRonde.getAfgerondReden() != null)
			{
				if (Bevolkingsonderzoek.COLON == screeningRonde.getBevolkingsonderzoek()
					&& colonVerwerkVerslagService.rondeHeeftDefinitiefMdlVervolgbeleid((ColonScreeningRonde) screeningRonde))
				{
					screeningRondeGebeurtenis.setExtraOmschrijving("definitieve diagnose");
				}
				else
				{
					screeningRondeGebeurtenis.setExtraOmschrijving(screeningRonde.getAfgerondReden());
				}
			}
			return screeningRondeGebeurtenis;
		}
		return null;
	}

	private List<ScreeningRondeGebeurtenissen> getCervixScreeningRondeGebeurtenissen(Client client, boolean alleenLaatste)
	{
		List<ScreeningRondeGebeurtenissen> dossiers = new ArrayList<>();
		if (client.getCervixDossier() != null)
		{
			List<CervixScreeningRonde> screeningRondes = new ArrayList<>(client.getCervixDossier().getScreeningRondes());
			Collections.sort(screeningRondes, (o1, o2) -> o1.getCreatieDatum().compareTo(o2.getCreatieDatum()) * -1);

			CervixCISHistorie cisHistorie = client.getCervixDossier().getCisHistorie();

			for (CervixScreeningRonde cervixScreeningRonde : screeningRondes)
			{

				int rondeNr = rondeNummerService.geefRondeNummer(cervixScreeningRonde);

				ScreeningRondeGebeurtenissen rondeDossier = new ScreeningRondeGebeurtenissen(rondeNr);
				rondeDossier.setScreeningRonde(cervixScreeningRonde);
				hibernateService.reload(cervixScreeningRonde);

				if (cisHistorie != null && cervixScreeningRonde.equals(cisHistorie.getScreeningRonde()) && cisHistorie.isHeeftPap0())
				{
					ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
					screeningRondeGebeurtenis.setDatum(cervixScreeningRonde.getCreatieDatum());
					screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.CIS_PAP0);
					screeningRondeGebeurtenis.setBron(GebeurtenisBron.AUTOMATISCH);
					rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
				}

				maakCervixUitstelGebeurtenissen(cervixScreeningRonde, rondeDossier);

				for (CervixUitnodiging cervixUitnodiging : cervixScreeningRonde.getUitnodigingen())
				{
					if (cervixUitnodiging.getMonsterType() == CervixMonsterType.ZAS)
					{
						ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
						screeningRondeGebeurtenis.setDatum(cervixUitnodiging.getCreatieDatum());
						screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.BMHK_ZAS_AANGEVRAAGD);
						screeningRondeGebeurtenis.setGebeurtenis(cervixUitnodiging.getBrief().getBriefType() == BriefType.CERVIX_ZAS_UITNODIGING ?
							TypeGebeurtenis.BMHK_ZAS_AANGEVRAAGD : TypeGebeurtenis.BMHK_ZAS_AANGEMAAKT);
						screeningRondeGebeurtenis.setBrief(cervixUitnodiging.getBrief());
						screeningRondeGebeurtenis.setUitnodiging(cervixUitnodiging);
						screeningRondeGebeurtenis.setExtraOmschrijving("Uitnodiging-id: " + cervixUitnodiging.getUitnodigingsId(),
							"te versturen op: " + Constants.getDateFormat().format(cervixUitnodiging.getUitnodigingsDatum()));
						screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(cervixUitnodiging));
						rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
						if (cervixUitnodiging.getVerstuurdDatum() != null)
						{
							screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
							screeningRondeGebeurtenis.setDatum(cervixUitnodiging.getVerstuurdDatum());
							screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.BMHK_ZAS_KLAARGEZET);
							screeningRondeGebeurtenis.setBrief(cervixUitnodiging.getBrief());
							screeningRondeGebeurtenis.setUitnodiging(cervixUitnodiging);
							screeningRondeGebeurtenis.setExtraOmschrijving("Uitnodiging-id: " + cervixUitnodiging.getUitnodigingsId(), cervixUitnodiging.getTemplateNaam());
							screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(cervixUitnodiging));
							rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
						}
						if (cervixUitnodiging.getGeannuleerdDatum() != null)
						{
							screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
							screeningRondeGebeurtenis.setDatum(cervixUitnodiging.getGeannuleerdDatum());
							screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.BMHK_ZAS_GEANNULEERD);
							screeningRondeGebeurtenis.setUitnodiging(cervixUitnodiging);
							screeningRondeGebeurtenis.setExtraOmschrijving("Uitnodiging-id: " + cervixUitnodiging.getUitnodigingsId());
							screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(cervixUitnodiging));
							rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
						}

						CervixZas zas = (CervixZas) cervixUitnodiging.getMonster();
						if (zas != null)
						{
							List<String> extraOmschrijvingen = new ArrayList<>();
							extraOmschrijvingen.add("Uitnodiging-id: " + zas.getUitnodiging().getUitnodigingsId());
							extraOmschrijvingen.add("Monster-id: " + zas.getMonsterId());
							extraOmschrijvingen.add(zas.getUitnodiging().getTemplateNaam());

							screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
							screeningRondeGebeurtenis.setDatum(zas.getVerstuurd());
							screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.BMHK_ZAS_SAMENGESTELD);
							screeningRondeGebeurtenis.setUitnodiging(zas.getUitnodiging());
							screeningRondeGebeurtenis.setExtraOmschrijving(extraOmschrijvingen.toArray(new String[] {}));
							screeningRondeGebeurtenis.setBron(GebeurtenisBron.AUTOMATISCH);

							rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
						}
					}

					retouren(rondeDossier, cervixUitnodiging);
				}

				List<CervixMonster> ontvangenMonsters = cervixRondeDao.getOntvangenMonsters(cervixScreeningRonde);
				for (CervixMonster ontvangenMonster : ontvangenMonsters)
				{
					CervixUitnodiging cervixUitnodiging = ontvangenMonster.getUitnodiging();

					for (CervixHpvBeoordeling beoordeling : ontvangenMonster.getHpvBeoordelingen())
					{
						ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
						screeningRondeGebeurtenis.setDatum(beoordeling.getHpvBericht().getOntvangen());
						screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.BMHK_MONSTER_HPV_GEANALYSEERD);
						screeningRondeGebeurtenis.setUitnodiging(cervixUitnodiging);
						screeningRondeGebeurtenis.setBeoordeling(beoordeling);
						List<String> extraOmschrijving = new ArrayList<>();
						extraOmschrijving.add("Monster-id: ");
						extraOmschrijving.add(ontvangenMonster.getMonsterId());
						extraOmschrijving.add("Beoordeling: ");
						extraOmschrijving.add(beoordeling.getHpvUitslag().getNaam());
						if (ontvangenMonster.getVerwijderdDatum() != null)
						{
							extraOmschrijving.add("VERWIJDERD");
						}
						screeningRondeGebeurtenis.setExtraOmschrijving(extraOmschrijving.toArray(new String[] {}));
						screeningRondeGebeurtenis.setBron(GebeurtenisBron.AUTOMATISCH);
						rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
					}
					if (ontvangenMonster.getVerwijderdDatum() != null)
					{
						ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
						screeningRondeGebeurtenis.setDatum(ontvangenMonster.getVerwijderdDatum());
						screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.BMHK_RESULTATEN_MONSTER_VERWIJDERD);
						screeningRondeGebeurtenis.setUitnodiging(cervixUitnodiging);
						screeningRondeGebeurtenis.setExtraOmschrijving("Monster-id: ", ontvangenMonster.getMonsterId());
						screeningRondeGebeurtenis.setBron(GebeurtenisBron.MEDEWERKER);
						rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
					}
					if (CervixMonsterUtil.isUitstrijkje(ontvangenMonster))
					{
						CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) ontvangenMonster;
						addUitstrijkjeStatusGebeurtenissen(rondeDossier, uitstrijkje);

						CervixLabformulier labformulier = uitstrijkje.getLabformulier();
						if (labformulier != null)
						{
							addLabformulierStatusGebeurtenissen(rondeDossier, labformulier);
						}

						CervixCytologieOrder cytologieOrder = uitstrijkje.getCytologieOrder();
						if (cytologieOrder != null && uitstrijkje.getCytologieVerslag() == null && CervixCytologieOrderStatus.VERSTUURD == cytologieOrder.getStatus())
						{
							ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
							screeningRondeGebeurtenis.setDatum(cytologieOrder.getStatusDatum());
							screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.BMHK_ORDER_VERSTUURD);
							screeningRondeGebeurtenis.setUitnodiging(cervixUitnodiging);
							screeningRondeGebeurtenis.setExtraOmschrijving("Monster-id: ", ontvangenMonster.getMonsterId(), "Reden: ",
								EnumStringUtil.getPropertyString(uitstrijkje.getCytologieOrder().getCytologieReden()));
							screeningRondeGebeurtenis.setBron(GebeurtenisBron.AUTOMATISCH);
							rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
						}

						List<CervixHuisartsBericht> huisartsBerichten = new ArrayList<>();
						if (uitstrijkje.getHuisartsBericht() != null)
						{
							huisartsBerichten.add(uitstrijkje.getHuisartsBericht());
						}
						if (labformulier != null && labformulier.getUitstrijkjeOntbreektHuisartsBericht() != null)
						{
							huisartsBerichten.add(labformulier.getUitstrijkjeOntbreektHuisartsBericht());
						}
						for (CervixHuisartsBericht huisartsBericht : huisartsBerichten)
						{
							voegHuisartsberichtGebeurtenisToe(rondeDossier, huisartsBericht, huisartsBericht.getAanmaakDatum(), TypeGebeurtenis.BMHK_HUISARTSBERICHT_AANGEMAAKT,
								GebeurtenisBron.AUTOMATISCH);

							if (huisartsBericht.getStatus() != CervixHuisartsBerichtStatus.AANGEMAAKT)
							{
								TypeGebeurtenis type;
								GebeurtenisBron bron;
								switch (huisartsBericht.getStatus())
								{
								case VERSTUURD:
									type = TypeGebeurtenis.BMHK_HUISARTSBERICHT_VERSTUURD;
									bron = GebeurtenisBron.AUTOMATISCH;
									break;
								case OPNIEUW_VERSTUURD:
									type = TypeGebeurtenis.BMHK_HUISARTSBERICHT_OPNIEUW_VERSTUURD;
									bron = GebeurtenisBron.MEDEWERKER;
									break;
								case VERSTUREN_MISLUKT:
									type = TypeGebeurtenis.BMHK_HUISARTSBERICHT_VERSTUREN_MISLUKT;
									bron = GebeurtenisBron.AUTOMATISCH;
									break;
								case KLANTNUMMER_NIET_GEVERIFIEERD:
									type = TypeGebeurtenis.BMHK_HUISARTSBERICHT_KLANTNUMMER_NIET_GEVERIFIEERD;
									bron = GebeurtenisBron.AUTOMATISCH;
									break;
								case OPNIEUW_VERSTUREN_MISLUKT:
									type = TypeGebeurtenis.BMHK_HUISARTSBERICHT_OPNIEUW_VERSTUREN_MISLUKT;
									bron = GebeurtenisBron.MEDEWERKER;
									break;
								case HUISARTS_ONBEKEND:
									type = TypeGebeurtenis.BMHK_HUISARTSBERICHT_HA_ONBEKEND;
									bron = GebeurtenisBron.AUTOMATISCH;
									break;
								default:
									throw new IllegalStateException();
								}

								voegHuisartsberichtGebeurtenisToe(rondeDossier, huisartsBericht, huisartsBericht.getStatusDatum(), type, bron);
								voegExtraHuisartsLocatieGebeurtenisToe(rondeDossier, huisartsBericht);
							}
						}
					}

					if (ontvangenMonster instanceof CervixZas)
					{
						addZasStatusGebeurtenissen(rondeDossier, CervixMonsterUtil.getZAS(ontvangenMonster));
					}
				}

				voegBrievenGebeurtenissenToe(cervixScreeningRonde, rondeDossier);

				rondeDossier.addGebeurtenissen(getScreeningRondeAfmeldingHeraanmeldingGebeurtenissen(cervixScreeningRonde));
				rondeDossier.addGebeurtenis(getScreeningRondeStatus(cervixScreeningRonde));

				dossiers.add(rondeDossier);
				if (alleenLaatste)
				{
					break;
				}
			}
		}
		return dossiers;
	}

	private void maakCervixUitstelGebeurtenissen(CervixScreeningRonde cervixScreeningRonde, ScreeningRondeGebeurtenissen rondeDossier)
	{
		CervixUitstel huidigeUitstel = null;
		for (Object auditRondeRow : EntityAuditUtil.getEntityHistory(cervixScreeningRonde, hibernateService.getHibernateSession(), AuditEntity.property("uitstel").isNotNull(),
			true))
		{
			CervixScreeningRonde auditRonde = EntityAuditUtil.getRevisionEntity(auditRondeRow);

			CervixUitstel uitstel = auditRonde.getUitstel();
			for (Object auditUitstelRow : EntityAuditUtil.getEntityHistory(uitstel, hibernateService.getHibernateSession(), false))
			{
				CervixUitstel auditUitstel = EntityAuditUtil.getRevisionEntity(auditUitstelRow);
				if (EntityAuditUtil.getRevisionType(auditUitstelRow) != RevisionType.DEL && (huidigeUitstel == null || !huidigeUitstel.equals(auditUitstel)))
				{
					ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
					screeningRondeGebeurtenis.setDatum(auditUitstel.getWijzigingsDatum());
					screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.UITSTEL);
					List<String> extraOmschrijvingen = new ArrayList<>();
					extraOmschrijvingen.add("Tot: " + Constants.getDateFormat().format(auditUitstel.getUitstellenTotDatum()));
					extraOmschrijvingen.add("Reden: ");
					extraOmschrijvingen.add(EnumStringUtil.getPropertyString(auditUitstel.getUitstelType()));
					Date geannuleerdDatum = auditUitstel.getGeannuleerdDatum();
					if (geannuleerdDatum != null)
					{
						extraOmschrijvingen.add("Geannuleerd op: " + Constants.getDateFormat().format(geannuleerdDatum));
					}
					screeningRondeGebeurtenis.setExtraOmschrijving(extraOmschrijvingen.toArray(new String[] {}));

					screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(auditUitstel, false));
					rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
					huidigeUitstel = uitstel;
					break;
				}
			}
		}
	}

	private void voegExtraHuisartsLocatieGebeurtenisToe(ScreeningRondeGebeurtenissen rondeDossier, CervixHuisartsBericht huisartsBericht)
	{
		if (huisartsBericht.getExtraHuisartsLocatieVerstuurdDatum() != null)
		{
			voegHuisartsberichtGebeurtenisToe(rondeDossier, huisartsBericht, huisartsBericht.getExtraHuisartsLocatieVerstuurdDatum(),
				TypeGebeurtenis.BMHK_HUISARTSBERICHT_VERSTUURD_EXTRA_HUISARTS, GebeurtenisBron.MEDEWERKER);
		}
	}

	private List<ScreeningRondeGebeurtenissen> getMammaScreeningRondeGebeurtenissen(Client client, boolean alleenLaatste)
	{
		List<ScreeningRondeGebeurtenissen> dossiers = new ArrayList<>();
		if (client.getMammaDossier() != null)
		{
			List<MammaScreeningRonde> screeningRondes = new ArrayList<>(client.getMammaDossier().getScreeningRondes());
			Collections.sort(screeningRondes, (o1, o2) -> o1.getCreatieDatum().compareTo(o2.getCreatieDatum()) * -1);

			int rondeNr = screeningRondes.size();

			for (MammaScreeningRonde screeningRonde : screeningRondes)
			{

				ScreeningRondeGebeurtenissen rondeDossier = new ScreeningRondeGebeurtenissen(rondeNr--);
				rondeDossier.setScreeningRonde(screeningRonde);
				rondeDossier.addGebeurtenis(getScreeningRondeStatus(screeningRonde));

				voegAfmeldingenHeraanmeldingenToe(screeningRonde, rondeDossier);
				voegBrievenGebeurtenissenToe(screeningRonde, rondeDossier);
				voegMammaUitnodigingGebeurtenissenToe(screeningRonde, rondeDossier);
				voegMammaUitstelGebeurtenissenToe(screeningRonde, rondeDossier);
				voegUploadBeeldenVerzoekGebeurtenissenToe(screeningRonde, rondeDossier);
				voegHuisartsToegevoegdGebeurtenisToe(screeningRonde, rondeDossier);
				voegDigitaalClientBerichtGebeurtenissenToe(screeningRonde, rondeDossier);
				voegFollowUpGebeurtenissenToe(screeningRonde, rondeDossier);

				dossiers.add(rondeDossier);
				if (alleenLaatste)
				{
					break;
				}
			}
		}
		return dossiers;
	}

	private void voegAfmeldingenHeraanmeldingenToe(MammaScreeningRonde screeningRonde, ScreeningRondeGebeurtenissen rondeDossier)
	{
		rondeDossier.addGebeurtenissen(getScreeningRondeAfmeldingHeraanmeldingGebeurtenissen(screeningRonde));
	}

	private void voegFollowUpGebeurtenissenToe(MammaScreeningRonde screeningRonde, ScreeningRondeGebeurtenissen rondeDossier)
	{
		if (screeningRonde.getFollowUpConclusieStatus() != null)
		{
			rondeDossier.addGebeurtenis(maakGebeurtenisFollowUpConclusie(screeningRonde));
		}

		voegFollowUpRadiologieVerslagGebeurtenissenToe(screeningRonde, rondeDossier);
		voegFollowUpPaVerslagGebeurtenissenToe(screeningRonde, rondeDossier);
	}

	private void voegMammaUitnodigingGebeurtenissenToe(MammaScreeningRonde screeningRonde, ScreeningRondeGebeurtenissen rondeDossier)
	{
		for (MammaUitnodiging uitnodiging : screeningRonde.getUitnodigingen())
		{
			for (MammaAfspraak afspraak : uitnodiging.getAfspraken())
			{
				ScreeningRondeGebeurtenis gebeurtenis = new ScreeningRondeGebeurtenis();

				maakGebeurtenisMammaAfspraak(afspraak, gebeurtenis);

				rondeDossier.addGebeurtenis(gebeurtenis);
				MammaOnderzoek onderzoek = afspraak.getOnderzoek();
				if (onderzoek != null && onderzoek.isDoorgevoerd())
				{
					rondeDossier.addGebeurtenis(maakMammaOnderzoekGebeurtenis(onderzoek));
					if (onderzoek.getOnvolledigOnderzoek() != null && onderzoek.getOnderbrokenOnderzoek() != null)
					{
						gebeurtenis = maakGebeurtenisMammaOnderbrokenOnderzoekAutomatischNaarOnvolledig(onderzoek);
						if (GebeurtenisBron.MEDEWERKER.equals(gebeurtenis.getBron()))
						{
							rondeDossier.addGebeurtenis(maakGebeurtenisMammaOnderbrokenZonderVervolg(onderzoek));
						}
						rondeDossier.addGebeurtenis(gebeurtenis);
					}
					if (onderzoek.getStatus() == MammaOnderzoekStatus.ONDERBROKEN_ZONDER_VERVOLG)
					{
						rondeDossier.addGebeurtenis(maakGebeurtenisMammaOnderbrokenZonderVervolg(onderzoek));
					}
					voegMammaBeoordelingGebeurtenissenToe(rondeDossier, onderzoek);
				}
			}
		}
	}

	private void voegMammaBeoordelingGebeurtenissenToe(ScreeningRondeGebeurtenissen rondeDossier, MammaOnderzoek onderzoek)
	{
		for (MammaBeoordeling beoordeling : onderzoek.getBeoordelingen())
		{
			maakBeoordelingGebeurtenissen(rondeDossier, beoordeling);

			AuditQuery query = EntityAuditUtil.createQuery(beoordeling, hibernateService.getHibernateSession());

			query.addOrder(AuditEntity.revisionNumber().desc());

			List<?> beoordelingAudits = query.getResultList();
			MammaBeoordeling prevBeoordeling = null;
			boolean alEerderAuditRecordVerslaglezingGevonden = false;
			boolean alEerderAuditRecordOpgeschorteBeoordelingGevonden = false;
			for (Object beoordelingAudit : beoordelingAudits)
			{
				final MammaBeoordeling beoordelingHistorisch = EntityAuditUtil.getRevisionEntity(beoordelingAudit);
				if (prevBeoordeling != null && !beoordelingHistorisch.getStatus().equals(prevBeoordeling.getStatus()))
				{
					if (MammaBeoordelingStatus.OPGESCHORT.equals(beoordelingHistorisch.getStatus())
						&& !beoordelingHistorisch.getStatus().equals(prevBeoordeling.getStatus()))
					{
						if (alEerderAuditRecordOpgeschorteBeoordelingGevonden)
						{
							rondeDossier.addGebeurtenis(maakBeoordelingOpgeschortGebeurtenis(beoordelingHistorisch));
						}
						alEerderAuditRecordOpgeschorteBeoordelingGevonden = true;
					}
					if (MammaBeoordelingStatus.VERSLAG_GEREED.equals(beoordelingHistorisch.getStatus()))
					{
						if (alEerderAuditRecordVerslaglezingGevonden)
						{
							rondeDossier.addGebeurtenis(maakVerslagGebeurtenis(beoordelingHistorisch, TypeGebeurtenis.MAMMA_VERWIJSVERSLAG_GEREED));
						}
						alEerderAuditRecordVerslaglezingGevonden = true;
					}
					if (MammaBeoordelingStatus.VERSLAG_AFGEKEURD.equals(beoordelingHistorisch.getStatus()))
					{ 
						rondeDossier.addGebeurtenis(maakAfgekeurdBeoordelingGebeurtenis(beoordelingHistorisch));
					}
				}
				prevBeoordeling = beoordelingHistorisch;
			}

			rondeDossier.addGebeurtenissen(beoordeling.getHuisartsBerichten()
				.stream()
				.map(this::maakMammaHuisartsberichtGebeurtenis)
				.collect(Collectors.toList()));
		}
	}

	private void voegHuisartsToegevoegdGebeurtenisToe(MammaScreeningRonde screeningRonde, ScreeningRondeGebeurtenissen rondeDossier)
	{
		if ((screeningRonde.getHuisarts() != null || screeningRonde.getGeenHuisartsOptie() != null) && screeningRonde.getDatumVastleggenHuisarts() != null)
		{
			rondeDossier.addGebeurtenis(maakHuisartsToegevoegd(screeningRonde, TypeGebeurtenis.MAMMA_HUISARTS_TOEGEVOEGD,
				screeningRonde.getDatumVastleggenHuisarts()));
		}
	}

	private void voegMammaUitstelGebeurtenissenToe(MammaScreeningRonde screeningRonde, ScreeningRondeGebeurtenissen rondeDossier)
	{
		for (MammaUitstel uitstel : screeningRonde.getUitstellen())
		{
			TypeGebeurtenis typeGebeurtenis;
			switch (uitstel.getUitstelReden())
			{
			case ACHTERVANG_UITSTEL:
				typeGebeurtenis = TypeGebeurtenis.MAMMA_ACHTERVANG_UITSTEL;
				break;
			case MINDER_VALIDE_UITWIJK_UITSTEL:
				typeGebeurtenis = TypeGebeurtenis.MAMMA_MINDER_VALIDE_UITWIJK;
				break;
			default:
				typeGebeurtenis = TypeGebeurtenis.UITSTEL;
			}

			ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
			screeningRondeGebeurtenis.setDatum(uitstel.getGemaaktOp());
			screeningRondeGebeurtenis.setGebeurtenis(typeGebeurtenis);
			List<String> extraOmschrijvingen = new ArrayList<>();
			extraOmschrijvingen.add("Streefdatum: " + Constants.getDateFormat().format(uitstel.getStreefDatum()));
			extraOmschrijvingen.add("Standplaats: " + uitstel.getStandplaats().getNaam());
			Date geannuleerdDatum = uitstel.getGeannuleerdOp();
			if (geannuleerdDatum != null)
			{
				extraOmschrijvingen.add("Geannuleerd op: " + Constants.getDateTimeFormat().format(geannuleerdDatum));

				if (uitstel.getGeannuleerdReden() != null)
				{
					switch (uitstel.getGeannuleerdReden())
					{
					case AFMELDING:
						extraOmschrijvingen.add("Geannuleerd reden: definitieve/eenmalige afmelding");
						break;
					case TEHUIS_KOPPELING:
						extraOmschrijvingen.add("Geannuleerd reden: geannuleerd vanwege koppelen aan tehuis");
						break;
					case MINDER_VALIDE_ONDERZOEK_ZIEKENHUIS:
						extraOmschrijvingen.add("Geannuleerd reden: mindervalide onderzoek in ziekenhuis");
						break;
					case NIEUWE_AFSPRAAK:
						extraOmschrijvingen.add("Geannuleerd reden: geannuleerd vanwege nieuwe afspraak");
						break;
					}
				}
			}

			screeningRondeGebeurtenis.setExtraOmschrijving(extraOmschrijvingen.toArray(new String[] {}));

			screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(uitstel, false));
			rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
		}
	}

	private void voegFollowUpRadiologieVerslagGebeurtenissenToe(MammaScreeningRonde screeningRonde, ScreeningRondeGebeurtenissen rondeDossier)
	{
		for (MammaFollowUpRadiologieVerslag followUpRadiologieVerslag : screeningRonde.getFollowUpRadiologieVerslagen())
		{
			if (followUpRadiologieVerslag.getIngevoerdOp() != null)
			{
				MammaFollowUpRadiologieVerslagGebeurtenis screeningRondeGebeurtenis = new MammaFollowUpRadiologieVerslagGebeurtenis(followUpRadiologieVerslag);
				screeningRondeGebeurtenis.setDatum(followUpRadiologieVerslag.getIngevoerdOp());
				screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.MAMMA_FOLLOW_UP_RADIOLOGIE_VERSLAG);
				screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(followUpRadiologieVerslag, false));

				rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
			}
		}
	}

	private void voegFollowUpPaVerslagGebeurtenissenToe(MammaScreeningRonde screeningRonde, ScreeningRondeGebeurtenissen rondeDossier)
	{
		for (MammaFollowUpVerslag followUpVerslag : baseFollowUpService.getFollowUpVerslagenZonderLandelijkeMonitor(screeningRonde))
		{
			if (VerslagStatus.AFGEROND.equals(followUpVerslag.getStatus()))
			{
				ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
				screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.MAMMA_FOLLOW_UP_PATHOLOGIE_VERSLAG);
				screeningRondeGebeurtenis.setDatum(followUpVerslag.getDatumVerwerkt());
				screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(followUpVerslag, false));
				screeningRondeGebeurtenis.setVerslag(followUpVerslag);
				screeningRondeGebeurtenis.setClickable(true);

				rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
			}
		}
	}

	private void voegUploadBeeldenVerzoekGebeurtenissenToe(MammaScreeningRonde screeningRonde, ScreeningRondeGebeurtenissen rondeDossier)
	{
		for (MammaUploadBeeldenVerzoek uploadBeeldenVerzoek : screeningRonde.getUploadBeeldenVerzoeken())
		{
			MammaUploadBeeldenVerzoekGebeurtenis uploadBeeldenVerzoekGebeurtenis = new MammaUploadBeeldenVerzoekGebeurtenis(uploadBeeldenVerzoek);
			uploadBeeldenVerzoekGebeurtenis.setGebeurtenis(TypeGebeurtenis.MAMMA_UPLOAD_BEELDEN_VERZOEK);
			uploadBeeldenVerzoekGebeurtenis.setDatum(uploadBeeldenVerzoek.getStatusDatum());
			uploadBeeldenVerzoekGebeurtenis.setBron(bepaalGebeurtenisBron(uploadBeeldenVerzoek, false));
			MammaUploadBeeldenPoging uploadBeeldenPoging = uploadBeeldenVerzoek.getLaatsteUploadPoging();
			String accessionNumber = uploadBeeldenPoging != null && uploadBeeldenPoging.getAccessionNumber() != null ? uploadBeeldenPoging.getAccessionNumber().toString() : null;
			String accessionNumberString = accessionNumber != null ? "accessionnummer: " + accessionNumber : null;
			String ziekenhuisNaam = uploadBeeldenVerzoek.getZiekenhuis().getNaam();
			uploadBeeldenVerzoekGebeurtenis.setExtraOmschrijving(
				EnumStringUtil.getPropertyString(uploadBeeldenPoging != null ? uploadBeeldenPoging.getIlmStatus() : null),
				"Status: ", EnumStringUtil.getPropertyString(uploadBeeldenVerzoek.getStatus()), accessionNumberString,
				"ziekenhuis: " + ziekenhuisNaam);
			rondeDossier.addGebeurtenis(uploadBeeldenVerzoekGebeurtenis);
		}
	}

	private ScreeningRondeGebeurtenis maakGebeurtenisFollowUpConclusie(MammaScreeningRonde screeningRonde)
	{
		MammaFollowUpConclusieStatus followUpConclusieStatus = screeningRonde.getFollowUpConclusieStatus();
		ScreeningRondeGebeurtenis screeningRondeGebeurtenis = null;
		if (followUpConclusieStatus != null)
		{
			screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
			screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.MAMMA_FOLLOW_UP_CONCLUSIE);
			screeningRondeGebeurtenis.setBron(GebeurtenisBron.AUTOMATISCH);
			screeningRondeGebeurtenis.setDatum(screeningRonde.getFollowUpConclusieStatusGewijzigdOp());
			screeningRondeGebeurtenis.setExtraOmschrijving("Conclusie: ", EnumStringUtil.getPropertyString(screeningRonde.getFollowUpConclusieStatus()));
		}

		return screeningRondeGebeurtenis;
	}

	private ScreeningRondeGebeurtenis maakGebeurtenisMammaOnderbrokenZonderVervolg(MammaOnderzoek onderzoek)
	{
		MammaOnderzoekGebeurtenis gebeurtenis = new MammaOnderzoekGebeurtenis(onderzoek);

		AuditReader auditReader = AuditReaderFactory.get(hibernateService.getHibernateSession());
		List<Number> revisionNumbers = auditReader.getRevisions(MammaOnderzoek.class, onderzoek.getId());
		if (revisionNumbers.isEmpty())
		{
			gebeurtenis.setDatum(onderzoek.getCreatieDatum()); 
		}
		else
		{
			gebeurtenis.setDatum(auditReader.getRevisionDate(revisionNumbers.get(revisionNumbers.size() - 1)));
		}
		gebeurtenis.setGebeurtenis(TypeGebeurtenis.MAMMA_ONDERZOEK_ONDERBROKEN_GEEN_VERVOLG);
		gebeurtenis.setBron(bepaalGebeurtenisBron(onderzoek, true));
		return gebeurtenis;
	}

	private MammaOnderzoekGebeurtenis maakMammaOnderzoekGebeurtenis(MammaOnderzoek onderzoek)
	{
		final MammaOnderzoekGebeurtenis onderzoekGebeurtenis = new MammaOnderzoekGebeurtenis(onderzoek);
		onderzoekGebeurtenis.setDatum(onderzoek.getCreatieDatum());
		if (onderzoek.getOnderbrokenOnderzoek() != null)
		{
			onderzoekGebeurtenis.setGebeurtenis(TypeGebeurtenis.MAMMA_ONDERZOEK_ONDERBROKEN);
			onderzoekGebeurtenis.setExtraOmschrijving("Reden: ", EnumStringUtil.getPropertyString(onderzoek.getOnderbrokenOnderzoek()));
		}
		else if (onderzoek.getOnvolledigOnderzoek() != null)
		{
			onderzoekGebeurtenis.setGebeurtenis(TypeGebeurtenis.MAMMA_ONDERZOEK_ONVOLLEDIG);
			onderzoekGebeurtenis.setExtraOmschrijving("Reden: ", EnumStringUtil.getPropertyString(onderzoek.getOnvolledigOnderzoek()));
		}
		else
		{
			onderzoekGebeurtenis.setGebeurtenis(TypeGebeurtenis.MAMMA_ONDERZOEK_AFGEROND);
		}
		MammaMammografie mammografie = onderzoek.getMammografie();
		if (mammografie != null)
		{
			onderzoekGebeurtenis.addToExtraOmschrijving(EnumStringUtil.getPropertyString(mammografie.getIlmStatus()));
		}
		onderzoekGebeurtenis.setBron(GebeurtenisBron.MEDEWERKER);
		return onderzoekGebeurtenis;
	}

	private void maakBeoordelingGebeurtenissen(ScreeningRondeGebeurtenissen rondeDossier, MammaBeoordeling beoordeling)
	{
		if (beoordeling.getEersteLezing() != null && !MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN.equals(beoordeling.getStatus())) 
		{
			rondeDossier.addGebeurtenis(maakLezingGebeurtenis(beoordeling, beoordeling.getEersteLezing(), TypeGebeurtenis.MAMMA_EERSTE_BEOORDELING_AFGEROND));
		}
		if (beoordeling.getTweedeLezing() != null && !MammaBeoordelingStatus.TWEEDE_LEZING_OPGESLAGEN.equals(beoordeling.getStatus())) 
		{
			rondeDossier.addGebeurtenis(maakLezingGebeurtenis(beoordeling, beoordeling.getTweedeLezing(), TypeGebeurtenis.MAMMA_TWEEDE_BEOORDELING_AFGEROND));
		}
		if (beoordeling.getDiscrepantieLezing() != null)
		{
			rondeDossier.addGebeurtenis(bepaalEnMaakDiscrepantieGebeurtenis(beoordeling));
		}
		if (beoordeling.getArbitrageLezing() != null)
		{
			rondeDossier.addGebeurtenis(maakLezingGebeurtenis(beoordeling, beoordeling.getArbitrageLezing(), TypeGebeurtenis.MAMMA_ARBITRAGE_AFGEROND));
		}
		if (beoordeling.getVerslagLezing() != null && !MammaBeoordelingStatus.VERSLAG_MAKEN.equals(beoordeling.getStatus()))
		{
			rondeDossier.addGebeurtenis(maakLezingGebeurtenis(beoordeling, beoordeling.getVerslagLezing(), TypeGebeurtenis.MAMMA_VERWIJSVERSLAG_GEREED));
		}
		if (beoordeling.getVerslagLezing() != null && MammaBeoordelingStatus.UITSLAG_ONGUNSTIG.equals(beoordeling.getStatus()))
		{
			rondeDossier.addGebeurtenis(maakUitslagOngunstigGebeurtenis(beoordeling, TypeGebeurtenis.MAMMA_UITSLAG_ONGUNSTIG));
		}
		if (MammaBeoordelingStatus.GEANNULEERD.equals(beoordeling.getStatus()))
		{
			rondeDossier.addGebeurtenis(maakGeannuleerdeBeoordelingGebeurtenis(beoordeling));
		}
		if (MammaBeoordelingStatus.OPGESCHORT.equals(beoordeling.getStatus()) || MammaBeoordelingStatus.OPGESCHORT_MET_AFSPRAAK.equals(beoordeling.getStatus()))
		{
			rondeDossier.addGebeurtenis(maakBeoordelingOpgeschortGebeurtenis(beoordeling));
		}
	}

	private ScreeningRondeGebeurtenis maakBeoordelingOpgeschortGebeurtenis(MammaBeoordeling beoordeling)
	{
		ScreeningRondeGebeurtenis gebeurtenis = new ScreeningRondeGebeurtenis();
		gebeurtenis.setClickable(false);
		gebeurtenis.setDatum(beoordeling.getStatusDatum());
		gebeurtenis.setGebeurtenis(TypeGebeurtenis.MAMMA_BEOORDELING_OPGESCHORT);
		gebeurtenis.setBron(GebeurtenisBron.MEDEWERKER);
		gebeurtenis.setExtraOmschrijving("Opschort reden: ", EnumStringUtil.getPropertyString(beoordeling.getOpschortReden()));
		return gebeurtenis;
	}

	private ScreeningRondeGebeurtenis maakHuisartsToegevoegd(ScreeningRonde screeningRonde, TypeGebeurtenis typeGebeurtenis, Date datum)
	{
		ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
		screeningRondeGebeurtenis.setGebeurtenis(typeGebeurtenis);
		screeningRondeGebeurtenis.setScreeningsRonde(screeningRonde);

		ScreenitRevisionEntity laatsteWijzigingHuisarts = dossierAuditService.getLastRevision(screeningRonde,
			AuditEntity.and(AuditEntity.property("datumVastleggenHuisarts").isNotNull(),
				AuditEntity.or(AuditEntity.revisionProperty("client").isNotNull(), AuditEntity.revisionProperty("instellingGebruiker").isNotNull())));
		screeningRondeGebeurtenis.setDatum(datum);
		GebeurtenisBron bron = dossierAuditService.getGebeurtenisBron(laatsteWijzigingHuisarts, GebeurtenisBron.AUTOMATISCH);
		screeningRondeGebeurtenis.setBron(bron);
		return screeningRondeGebeurtenis;
	}

	private ScreeningRondeGebeurtenis maakMammaHuisartsberichtGebeurtenis(MammaHuisartsBericht huisartsBericht)
	{
		ScreeningRondeGebeurtenis gebeurtenis = new ScreeningRondeGebeurtenis();
		gebeurtenis.setClickable(true);
		gebeurtenis.setDatum(huisartsBericht.getStatusDatum());
		switch (huisartsBericht.getStatus())
		{
		case AANGEMAAKT:
			gebeurtenis.setGebeurtenis(TypeGebeurtenis.MAMMA_HUISARTSBERICHT_AANGEMAAKT);
			break;
		case VERSTUURD:
			gebeurtenis.setGebeurtenis(
				!huisartsBericht.isEenOpnieuwVerzondenBericht() ?
					TypeGebeurtenis.MAMMA_HUISARTSBERICHT_VERSTUURD :
					TypeGebeurtenis.MAMMA_HUISARTSBERICHT_OPNIEUW_VERSTUURD);
			break;
		case VERSTUREN_MISLUKT:
			gebeurtenis.setGebeurtenis(
				!huisartsBericht.isEenOpnieuwVerzondenBericht() ?
					TypeGebeurtenis.MAMMA_HUISARTSBERICHT_VERSTUREN_MISLUKT :
					TypeGebeurtenis.MAMMA_HUISARTSBERICHT_OPNIEUW_VERSTUREN_MISLUKT);
			break;
		}
		gebeurtenis.setBron(bepaalGebeurtenisBron(huisartsBericht));
		gebeurtenis.setExtraOmschrijving(huisartsBericht.getBerichtType().getNaam(), huisartsBericht.getHuisarts().getPraktijknaam());
		gebeurtenis.setMammaHuisartsBericht(huisartsBericht);
		return gebeurtenis;
	}

	private ScreeningRondeGebeurtenis maakGebeurtenisMammaOnderbrokenOnderzoekAutomatischNaarOnvolledig(MammaOnderzoek onderzoek)
	{
		MammaOnderzoekGebeurtenis gebeurtenis = new MammaOnderzoekGebeurtenis(onderzoek);

		AuditReader auditReader = AuditReaderFactory.get(hibernateService.getHibernateSession());
		List<Number> revisionNumbers = auditReader.getRevisions(MammaOnderzoek.class, onderzoek.getId());
		if (revisionNumbers.isEmpty())
		{
			gebeurtenis.setDatum(onderzoek.getCreatieDatum()); 
		}
		else
		{
			gebeurtenis.setDatum(auditReader.getRevisionDate(revisionNumbers.get(revisionNumbers.size() - 1)));
		}
		GebeurtenisBron bron = bepaalGebeurtenisBron(onderzoek, true);
		gebeurtenis.setGebeurtenis(TypeGebeurtenis.MAMMA_ONDERZOEK_VAN_ONDERBROKEN_NAAR_ONVOLLEDIG);
		if (bron == null) 
		{
			bron = GebeurtenisBron.MEDEWERKER;
		}
		gebeurtenis.setBron(bron);
		return gebeurtenis;
	}

	private void maakGebeurtenisMammaAfspraak(MammaAfspraak afspraak, ScreeningRondeGebeurtenis gebeurtenis)
	{
		List<String> extraOmschrijvingen = createMammaAfspraakExtraOmschrijving(afspraak);
		gebeurtenis.setExtraOmschrijving(extraOmschrijvingen.toArray(new String[] {}));
		gebeurtenis.setClickable(false);
		gebeurtenis.setDatum(afspraak.getCreatiedatum());
		gebeurtenis.setGebeurtenis(afspraak.isGeforceerdeAfspraak() ? TypeGebeurtenis.MAMMA_AFSPRAAK_FORCEREN : TypeGebeurtenis.MAMMA_AFSPRAAK);
		gebeurtenis.setBron(bepaalGebeurtenisBron(afspraak, false));
	}

	private ScreeningRondeGebeurtenis maakVerslagGebeurtenis(MammaBeoordeling beoordelingHistorisch, TypeGebeurtenis type)
	{
		ScreeningRondeGebeurtenis gebeurtenis = new ScreeningRondeGebeurtenis();
		gebeurtenis.setClickable(false);
		gebeurtenis.setDatum(beoordelingHistorisch.getStatusDatum());
		gebeurtenis.setGebeurtenis(type);
		gebeurtenis.setBron(GebeurtenisBron.MEDEWERKER);
		return gebeurtenis;
	}

	private ScreeningRondeGebeurtenis maakUitslagOngunstigGebeurtenis(MammaBeoordeling beoordelingHistorisch, TypeGebeurtenis type)
	{
		ScreeningRondeGebeurtenis gebeurtenis = new ScreeningRondeGebeurtenis();
		gebeurtenis.setClickable(true);
		gebeurtenis.setDatum(beoordelingHistorisch.getStatusDatum());
		gebeurtenis.setGebeurtenis(type);
		gebeurtenis.setBron(GebeurtenisBron.MEDEWERKER);
		return gebeurtenis;
	}

	private MammaBeoordelingGebeurtenis maakGeannuleerdeBeoordelingGebeurtenis(MammaBeoordeling beoordelingHistorisch)
	{
		MammaBeoordelingGebeurtenis gebeurtenis = new MammaBeoordelingGebeurtenis();
		gebeurtenis.setClickable(false);
		gebeurtenis.setDatum(beoordelingHistorisch.getStatusDatum());
		gebeurtenis.setGebeurtenis(TypeGebeurtenis.MAMMA_BEOORDELING_GEANNULEERD);
		gebeurtenis.setBron(GebeurtenisBron.MEDEWERKER);
		gebeurtenis.setMammaBeoordeling(beoordelingHistorisch);
		return gebeurtenis;
	}

	private ScreeningRondeGebeurtenis maakAfgekeurdBeoordelingGebeurtenis(MammaBeoordeling beoordelingHistorisch)
	{
		ScreeningRondeGebeurtenis gebeurtenis = new ScreeningRondeGebeurtenis();
		gebeurtenis.setClickable(false);
		gebeurtenis.setDatum(beoordelingHistorisch.getStatusDatum());
		gebeurtenis.setGebeurtenis(TypeGebeurtenis.MAMMA_VERWIJSVERSLAG_AFGEKEURD);
		gebeurtenis.setExtraOmschrijving("Beoordeling afgewezen door CE met afkeurreden: ", beoordelingHistorisch.getAfkeurreden());
		gebeurtenis.setBron(GebeurtenisBron.MEDEWERKER);

		return gebeurtenis;
	}

	private ScreeningRondeGebeurtenis bepaalEnMaakDiscrepantieGebeurtenis(MammaBeoordeling beoordeling)
	{
		final MammaLezing discrepantieLezing = beoordeling.getDiscrepantieLezing();
		return isAutomatischeDiscrepantie(beoordeling)
			? getAutomatischeLezingGebeurtenis(TypeGebeurtenis.MAMMA_DISCREPANTIE_AUTOMATISCH_AFGEROND, beoordeling.getTweedeLezing().getBeoordelingDatum())
			: maakLezingGebeurtenis(beoordeling, discrepantieLezing, TypeGebeurtenis.MAMMA_DISCREPANTIE_AFGEROND);
	}

	private boolean isAutomatischeDiscrepantie(MammaBeoordeling beoordeling)
	{
		return beoordeling.getDiscrepantieLezing().getBeoordelaar() == null;
	}

	private ArrayList<String> createMammaAfspraakExtraOmschrijving(MammaAfspraak afspraak)
	{
		String seNaam = afspraak.getStandplaatsPeriode().getScreeningsEenheid().getNaam();
		ArrayList<String> list = new ArrayList<>(
			Arrays.asList(Constants.getDateTimeFormat().format(afspraak.getVanaf()), seNaam));
		switch (afspraak.getStatus())
		{
		case VERPLAATST:
		case GEPLAND:
			if (afspraak.getVerzettenReden() == null)
			{
				list.add("ronde." + EnumStringUtil.getPropertyString(MammaAfspraakStatus.GEPLAND));
			}
			break;
		default:
			list.add("ronde." + EnumStringUtil.getPropertyString(afspraak.getStatus()));
			break;
		}
		if (afspraak.getVerzettenReden() != null)
		{
			list.add("Verzet: ");
			list.add(EnumStringUtil.getPropertyString(afspraak.getVerzettenReden()));
		}

		if (Boolean.TRUE.equals(afspraak.getUitnodiging().getScreeningRonde().isGeforceerd()))
		{
			Optional<MammaAfspraak> eersteAfspraak = afspraak.getUitnodiging().getScreeningRonde().getUitnodigingen().stream().map(MammaUitnodiging::getAfspraken)
				.flatMap(Collection::stream).min(Comparator.comparing(MammaAfspraak::getCreatiedatum));
			if (eersteAfspraak.isPresent() && eersteAfspraak.get().equals(afspraak))
			{
				list.add("Geforceerde ronde");
			}
		}
		return list;
	}

	private ScreeningRondeGebeurtenis maakLezingGebeurtenis(MammaBeoordeling beoordeling, MammaLezing lezing, TypeGebeurtenis typeGebeurtenis)
	{
		ScreeningRondeGebeurtenis gebeurtenis = new MammaLezingGebeurtenis(beoordeling, lezing);
		gebeurtenis.setClickable(false);
		gebeurtenis.setDatum(lezing.getBeoordelingDatum());
		gebeurtenis.setGebeurtenis(typeGebeurtenis);
		if (lezing.getBeoordelaar() != null)
		{
			gebeurtenis.setExtraOmschrijving("Beoordeling door: ", NaamUtil.getNaamGebruiker(lezing.getBeoordelaar().getMedewerker()), "BI-RADS: ",
				MammaScreeningRondeUtil.bepaalNaamBiradsWaarde(MammaZijde.RECHTER_BORST, lezing.getBiradsRechts()) +
					MammaScreeningRondeUtil.bepaalNaamBiradsWaarde(MammaZijde.LINKER_BORST, lezing.getBiradsLinks()));
		}
		gebeurtenis.setBron(GebeurtenisBron.MEDEWERKER);
		if (lezing.getBeperktBeoordeelbaarReden() != null)
		{
			gebeurtenis.addToExtraOmschrijving("beperkt beoordeelbaar wegens: ", EnumStringUtil.getPropertyString(lezing.getBeperktBeoordeelbaarReden()));
		}
		return gebeurtenis;
	}

	private ScreeningRondeGebeurtenis getAutomatischeLezingGebeurtenis(TypeGebeurtenis typeGebeurtenis, Date datum)
	{
		ScreeningRondeGebeurtenis gebeurtenis = new ScreeningRondeGebeurtenis();
		gebeurtenis.setClickable(false);
		gebeurtenis.setDatum(datum);
		gebeurtenis.setGebeurtenis(typeGebeurtenis);
		gebeurtenis.setExtraOmschrijving("Doorgezet naar arbitrage");
		gebeurtenis.setBron(GebeurtenisBron.AUTOMATISCH);

		return gebeurtenis;
	}

	private ScreeningRondeGebeurtenis voegHuisartsberichtGebeurtenisToe(ScreeningRondeGebeurtenissen rondeDossier, CervixHuisartsBericht huisartsBericht, Date datum,
		TypeGebeurtenis type, GebeurtenisBron bron)
	{
		ScreeningRondeGebeurtenis gebeurtenis = new ScreeningRondeGebeurtenis();
		rondeDossier.addGebeurtenis(gebeurtenis);

		CervixUitstrijkje uitstrijkje = huisartsBericht.getUitstrijkje() != null ? huisartsBericht.getUitstrijkje() : huisartsBericht.getLabformulier().getUitstrijkje();
		gebeurtenis.setUitnodiging(uitstrijkje.getUitnodiging());
		gebeurtenis.setExtraOmschrijving("Monster-id: ", uitstrijkje.getMonsterId(), " Bericht: ", huisartsBericht.getBerichtType().getNaam());
		gebeurtenis.setHuisartsBericht(huisartsBericht);
		gebeurtenis.setClickable(true);

		gebeurtenis.setDatum(datum);
		gebeurtenis.setGebeurtenis(type);
		gebeurtenis.setBron(bron);

		return gebeurtenis;
	}

	private <S extends ScreeningRonde<?, ?, ?, ?>> void voegDigitaalClientBerichtGebeurtenissenToe(S screeningRonde, ScreeningRondeGebeurtenissen rondeDossier)
	{
		for (var clientBericht : screeningRonde.getBerichten())
		{
			var rondeGebeurtenis = new ScreeningRondeGebeurtenis();
			rondeGebeurtenis.setDigitaalClientBericht(clientBericht);
			rondeGebeurtenis.setDatum(DateUtil.toUtilDate(clientBericht.getCreatieMoment()));
			rondeGebeurtenis.setBron(bepaalGebeurtenisBron(clientBericht));
			rondeGebeurtenis.setClickable(true);

			if (Boolean.TRUE.equals(clientBericht.getVerzendenGefaald()) && DigitaalBerichtType.SMS == clientBericht.getDigitaalBerichtTemplateType().getBerichtType())
			{
				rondeGebeurtenis.setClickable(false);
				rondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.SMS_VERSTUREN_GEFAALD);
				rondeGebeurtenis.setExtraOmschrijving(clientBericht.getOmschrijving());
			}
			else if (DigitaalBerichtType.EMAIL == clientBericht.getDigitaalBerichtTemplateType().getBerichtType())
			{
				rondeGebeurtenis.setGebeurtenis(Boolean.TRUE.equals(clientBericht.getIsHerzonden()) ? TypeGebeurtenis.MAIL_OPNIEUW_VERZONDEN : TypeGebeurtenis.MAIL_VERZONDEN);
			}
			else
			{
				rondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.SMS_VERZONDEN);
			}

			voegBvoSpecifiekeOmschrijvingToe(rondeGebeurtenis, screeningRonde, clientBericht);

			rondeDossier.addGebeurtenis(rondeGebeurtenis);
		}
	}

	private void voegBvoSpecifiekeOmschrijvingToe(ScreeningRondeGebeurtenis rondeGebeurtenis, ScreeningRonde screeningRonde, DigitaalClientBericht mail)
	{
		if (Bevolkingsonderzoek.MAMMA == screeningRonde.getBevolkingsonderzoek())
		{
			rondeGebeurtenis.addToExtraOmschrijving(EnumStringUtil.getPropertyString(mail.getDigitaalBerichtTemplateType()), "uitnodigingsnummer: ",
				((MammaScreeningRonde) screeningRonde).getUitnodigingsNr().toString());
		}
	}

	private <B extends ClientBrief<S, ?, ?>, S extends ScreeningRonde<?, B, ?, ?>> void voegBrievenGebeurtenissenToe(S screeningRonde,
		ScreeningRondeGebeurtenissen rondeDossier)
	{
		for (B brief : screeningRonde.getBrieven())
		{
			ClientBrief clientBrief = brief; 
			if (clientBrief instanceof CervixBrief)
			{
				CervixOmissieGebeurtenisFactory.createOmissieGebeurtenisIndienVanToepassing((CervixBrief) clientBrief, rondeDossier);
			}

			BriefType briefType = brief.getBriefType();
			ClientBrief herdruk = BriefUtil.getHerdruk(brief);
			if ((!BriefType.COLON_VOORAANKONDIGING.equals(briefType) || herdruk != null)
				&& !BriefType.COLON_UITNODIGING.equals(briefType)
				&& !BriefType.getCervixZasBrieven().contains(briefType))
			{
				ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
				List<String> extraOmschrijvingen = new ArrayList<>();
				if (brief.getProjectBrief() == null)
				{
					maakScreeningRondeGebeurtenisBrief(brief, herdruk, screeningRondeGebeurtenis, extraOmschrijvingen);
				}
				else
				{
					ProjectBrief projectBrief = brief.getProjectBrief();
					screeningRondeGebeurtenis.setBrief(projectBrief);
					screeningRondeGebeurtenis.setDatum(projectBrief.getCreatieDatum());
					screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(projectBrief));
					extraOmschrijvingen.add(EnumStringUtil.getPropertyString(briefType));
					if (projectBrief.getHerdruk() != null)
					{
						herdrukGebeurtenis(screeningRondeGebeurtenis, extraOmschrijvingen, projectBrief, TypeGebeurtenis.PROJECT_BRIEF_HERDRUK);
					}
					else if (herdruk != null)
					{
						herdrukGebeurtenis(screeningRondeGebeurtenis, extraOmschrijvingen, brief, TypeGebeurtenis.PROJECT_BRIEF_HERDRUK);
					}
					else if (projectBrief.isGegenereerd())
					{
						MergedBrieven<?> mergedBrieven = projectBrief.getMergedBrieven();
						screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(mergedBrieven));
						if (mergedBrieven.getPrintDatum() != null)
						{
							screeningRondeGebeurtenis.setDatum(mergedBrieven.getPrintDatum());
							screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.PROJECT_BRIEF_AFGEDRUKT);
						}
						else
						{
							screeningRondeGebeurtenis.setDatum(mergedBrieven.getCreatieDatum());
							screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.PROJECT_BRIEF_KLAARGEZET);
						}
						extraOmschrijvingen.add(brief.getTemplateNaam());
					}
					else if (projectBrief.isVervangen())
					{
						screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.PROJECT_BRIEF_VERVANGEN);
					}
					else if (projectBrief.isTegenhouden())
					{
						screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.PROJECT_BRIEF_TEGENHOUDEN);
					}
					else
					{
						screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.PROJECT_BRIEF_AANGEMAAKT);
					}
				}
				bvoSpecifiekeExtraOmschrijvingenVanuitBrief(screeningRonde, brief, briefType, extraOmschrijvingen);
				screeningRondeGebeurtenis.setExtraOmschrijving(extraOmschrijvingen.toArray(new String[] {}));
				rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
			}
		}
	}

	private <B extends ClientBrief<S, ?, ?>, S extends ScreeningRonde<?, B, ?, ?>> void maakScreeningRondeGebeurtenisBrief(B brief, ClientBrief herdruk,
		ScreeningRondeGebeurtenis screeningRondeGebeurtenis, List<String> extraOmschrijvingen)
	{
		screeningRondeGebeurtenis.setBrief(brief);
		screeningRondeGebeurtenis.setDatum(brief.getCreatieDatum());
		screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(brief));
		extraOmschrijvingen.add(EnumStringUtil.getPropertyString(brief.getBriefType()));
		if (herdruk != null)
		{
			herdrukGebeurtenis(screeningRondeGebeurtenis, extraOmschrijvingen, brief, TypeGebeurtenis.BRIEF_HERDRUK);
		}
		else if (brief.isGegenereerd())
		{
			MergedBrieven<?> mergedBrieven = brief.getMergedBrieven();
			if (mergedBrieven != null)
			{
				screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(mergedBrieven));
				if (mergedBrieven.getPrintDatum() != null)
				{
					screeningRondeGebeurtenis.setDatum(mergedBrieven.getPrintDatum());
					screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.BRIEF_AFGEDRUKT);
				}
				else
				{
					screeningRondeGebeurtenis.setDatum(mergedBrieven.getCreatieDatum());
					screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.BRIEF_KLAARGEZET);
				}
			}
			else
			{
				screeningRondeGebeurtenis.setBron(GebeurtenisBron.MEDEWERKER);
				screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.BRIEF_AFGEDRUKT);
			}
			extraOmschrijvingen.add(brief.getTemplateNaam());
		}
		else if (brief.isVervangen())
		{
			screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.BRIEF_VERVANGEN);
		}
		else if (brief.isTegenhouden())
		{
			screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.BRIEF_TEGENHOUDEN);
		}
		else
		{
			screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.BRIEF_AANGEMAAKT);
		}
		gebeurtenisAanvullen(brief, screeningRondeGebeurtenis, extraOmschrijvingen);
	}

	private <B extends ClientBrief<?, ?, ?>> void gebeurtenisAanvullen(B brief, ScreeningRondeGebeurtenis screeningRondeGebeurtenis, List<String> extraOmschrijvingen)
	{
		if (screeningRondeGebeurtenis.getGebeurtenis() == TypeGebeurtenis.BRIEF_AFGEDRUKT && brief instanceof CervixBrief)
		{
			var cervixBrief = (CervixBrief) brief;
			var uitnodiging = cervixBrief.getUitnodiging();
			if (uitnodiging != null && uitnodiging.getGecombineerdeZas() != null)
			{
				extraOmschrijvingen.add(0, "samen met ZAS");
			}
		}
	}

	private <B extends ClientBrief<?, ?, ?>> void herdrukGebeurtenis(ScreeningRondeGebeurtenis screeningRondeGebeurtenis, List<String> extraOmschrijvingen, B brief,
		TypeGebeurtenis typeGebeurtenis)
	{
		Date datum = brief.getCreatieDatum();

		MergedBrieven<?> mergedBrieven = BriefUtil.getMergedBrieven(brief);
		if (mergedBrieven != null)
		{
			if (Boolean.TRUE.equals(mergedBrieven.getGeprint()))
			{
				datum = mergedBrieven.getPrintDatum();
			}
			else
			{
				datum = mergedBrieven.getCreatieDatum();
			}
		}
		screeningRondeGebeurtenis.setDatum(datum);
		screeningRondeGebeurtenis.setGebeurtenis(typeGebeurtenis);
		extraOmschrijvingen.clear();
		extraOmschrijvingen.addAll(getExtraOmschrijvingenVoorHerdrukBrief(brief));
	}

	private <B extends ClientBrief<S, ?, ?>, S extends ScreeningRonde<?, B, ?, ?>> void bvoSpecifiekeExtraOmschrijvingenVanuitBrief(S screeningRonde, B brief,
		BriefType briefType,
		List<String> extraOmschrijvingen)
	{
		if (brief.getBevolkingsonderzoek() == Bevolkingsonderzoek.CERVIX)
		{
			CervixBrief cervixBrief = (CervixBrief) HibernateHelper.deproxy(brief);
			CervixUitnodiging uitnodiging = cervixBrief.getUitnodiging();
			if (uitnodiging != null && uitnodiging.getMonsterType() == CervixMonsterType.UITSTRIJKJE)
			{
				extraOmschrijvingen.add("Monster-id: ");
				extraOmschrijvingen.add(uitnodiging.getMonster().getMonsterId());
				extraOmschrijvingen.add("Controleletters: ");
				extraOmschrijvingen.add(((CervixUitstrijkje) uitnodiging.getMonster()).getControleLetters());
			}

			if (briefType == BriefType.CERVIX_UITNODIGING)
			{
				extraOmschrijvingen.add("Leeftijdcategorie: ");
				extraOmschrijvingen
					.add(Integer.toString(cervixBrief.getUitnodiging().getScreeningRonde().getLeeftijdcategorie().getLeeftijd()));
			}
		}
		else if (brief.getBevolkingsonderzoek() == Bevolkingsonderzoek.COLON)
		{
			ColonBrief colonBrief = (ColonBrief) HibernateHelper.deproxy(brief);
			IFOBTTest ifobtTest = colonBrief.getIfobtTest();
			if (ifobtTest != null)
			{
				ColonUitnodiging uitnodiging = FITTestUtil.getUitnodiging(ifobtTest);
				if (uitnodiging != null)
				{
					extraOmschrijvingen.add("UitnodigingsID: ");
					extraOmschrijvingen.add(String.valueOf(uitnodiging.getUitnodigingsId()));
				}
				extraOmschrijvingen.add("Barcode: ");
				extraOmschrijvingen.add(ifobtTest.getBarcode());
			}
		}
		else if (brief.getBevolkingsonderzoek() == Bevolkingsonderzoek.MAMMA)
		{
			ScreeningRonde sr = (ScreeningRonde) HibernateHelper.deproxy(screeningRonde);
			if (sr instanceof MammaScreeningRonde)
			{
				MammaScreeningRonde mammaScreeningRonde = (MammaScreeningRonde) sr;
				extraOmschrijvingen.add("uitnodigingsnummer: ");
				extraOmschrijvingen.add(mammaScreeningRonde.getUitnodigingsNr().toString());
				if (Boolean.TRUE.equals(mammaScreeningRonde.isGeforceerd()))
				{
					extraOmschrijvingen.add("Geforceerde ronde");
				}
			}
		}
	}

	private List<String> getExtraOmschrijvingenVoorHerdrukBrief(ClientBrief brief)
	{
		List<String> extraOmschrijvingen = new ArrayList<>();
		ClientBrief oudeBrief = BriefUtil.getHerdruk(brief);

		if (BriefUtil.isGegenereerd(brief))
		{
			MergedBrieven<?> mergedBrieven = BriefUtil.getMergedBrieven(brief);
			if (mergedBrieven == null || mergedBrieven.getPrintDatum() != null)
			{
				extraOmschrijvingen.add("Afgedrukt");
			}
			else
			{
				extraOmschrijvingen.add("Klaargezet");
			}
		}
		else if (brief.isVervangen())
		{
			extraOmschrijvingen.add("Vervangen");
		}
		else if (BriefUtil.isTegengehouden(brief))
		{
			extraOmschrijvingen.add("Tegengehouden");
		}
		else
		{
			extraOmschrijvingen.add("Aangemaakt");
		}

		extraOmschrijvingen.add(EnumStringUtil.getPropertyString(brief.getBriefType()));

		MergedBrieven mergedBrieven = BriefUtil.getMergedBrieven(oudeBrief);
		if (mergedBrieven != null)
		{
			Date correcteDatum = mergedBrieven.getPrintDatum();
			if (correcteDatum == null)
			{
				correcteDatum = mergedBrieven.getCreatieDatum();
			}
			SimpleDateFormat simpleDateFormat = Constants.getDateTimeSecondsFormat();
			extraOmschrijvingen.add("Herdruk van de brief die is verstuurd op: " + simpleDateFormat.format(correcteDatum));
		}
		return extraOmschrijvingen;
	}

	private void addUitstrijkjeStatusGebeurtenissen(ScreeningRondeGebeurtenissen rondeDossier, CervixUitstrijkje uitstrijkje)
	{
		CervixUitstrijkjeStatus currentStatus = null;
		Map<CervixUitstrijkjeStatus, Object> alleLaatsteStatussen = new EnumMap<>(CervixUitstrijkjeStatus.class);
		List uitstrijkjeHistory = EntityAuditUtil.getEntityHistory(uitstrijkje, hibernateService.getHibernateSession(), true);
		for (Object auditRow : uitstrijkjeHistory)
		{
			CervixUitstrijkje auditUitstrijkje = EntityAuditUtil.getRevisionEntity(auditRow);
			CervixUitstrijkjeStatus status = auditUitstrijkje.getUitstrijkjeStatus();
			long timestamp = EntityAuditUtil.getRevisionInfo(auditRow).getTimestamp();
			boolean toevoegen = true;
			for (Entry<CervixUitstrijkjeStatus, Object> newestAuditRow : alleLaatsteStatussen.entrySet())
			{
				CervixUitstrijkjeStatus status1 = newestAuditRow.getKey();
				long timestamp1 = EntityAuditUtil.getRevisionInfo(newestAuditRow.getValue()).getTimestamp();
				if (status.equals(status1) && timestamp1 > timestamp)
				{
					toevoegen = false;
					break;
				}
			}
			if (toevoegen)
			{
				alleLaatsteStatussen.put(status, auditRow);
			}
		}
		CervixCytologieVerslag cytologieVerslag = uitstrijkje.getCytologieVerslag();
		for (Object object : alleLaatsteStatussen.values())
		{
			CervixUitstrijkje auditedUitstrijkje = EntityAuditUtil.getRevisionEntity(object);
			if (currentStatus == null || !auditedUitstrijkje.getUitstrijkjeStatus().equals(currentStatus))
			{
				ScreenitRevisionEntity revisionEntity = EntityAuditUtil.getRevisionInfo(object);
				ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
				List<String> extraOmschrijvingen = new ArrayList<>();
				extraOmschrijvingen.add("Monster-id: " + uitstrijkje.getMonsterId());
				TypeGebeurtenis gebeurtenis = null;
				screeningRondeGebeurtenis.setDatum(auditedUitstrijkje.getStatusDatum());
				switch (auditedUitstrijkje.getUitstrijkjeStatus())
				{
				case ONTVANGEN:
					gebeurtenis = TypeGebeurtenis.BMHK_MONSTER_ONTVANGEN;
					break;
				case BEOORDEELD_DOOR_CYTOLOGIE:
					extraOmschrijvingen.add("Reden: ");
					extraOmschrijvingen.add(EnumStringUtil.getPropertyString(uitstrijkje.getCytologieOrder().getCytologieReden()));
					extraOmschrijvingen.add("Uitslag: ");
					extraOmschrijvingen.add(EnumStringUtil.getPropertyString(cytologieVerslag.getCytologieUitslag()));
					if (uitstrijkje.getVerwijderdDatum() != null)
					{
						extraOmschrijvingen.add("VERWIJDERD");
					}
					screeningRondeGebeurtenis.setVerslag(cytologieVerslag);
					gebeurtenis = TypeGebeurtenis.BMHK_CYTOLOGISCHE_BEOORDELING;
					break;
				case NIET_ANALYSEERBAAR:
					gebeurtenis = TypeGebeurtenis.BMHK_MONSTER_NIET_ANALYSEERBAAR;
					break;
				case NIET_ONTVANGEN:
					if (heeftStatusOntvangenGehadVoor(uitstrijkjeHistory, auditedUitstrijkje.getStatusDatum()))
					{
						gebeurtenis = TypeGebeurtenis.BMHK_MONSTER_NIET_ONTVANGEN;
					}
					break;
				default:
					break;
				}

				addRondeGebeurtenisToDossier(rondeDossier, uitstrijkje.getUitnodiging(), revisionEntity, screeningRondeGebeurtenis, extraOmschrijvingen, gebeurtenis);
			}
			currentStatus = auditedUitstrijkje.getUitstrijkjeStatus();
		}

		if (cytologieVerslag != null && CollectionUtils.isNotEmpty(cytologieVerslag.getHerzieningenOntvangen()))
		{
			for (Date herzieningOntvangen : cytologieVerslag.getHerzieningenOntvangen())
			{
				ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
				List<String> extraOmschrijvingen = new ArrayList<>();
				extraOmschrijvingen.add("Monster-id: " + uitstrijkje.getMonsterId());
				screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.BMHK_HERZIENING_CYTOLOGISCHE_BEOORDELING);
				screeningRondeGebeurtenis.setUitnodiging(uitstrijkje.getUitnodiging());
				screeningRondeGebeurtenis.setExtraOmschrijving(extraOmschrijvingen.toArray(new String[] {}));
				screeningRondeGebeurtenis.setBron(GebeurtenisBron.AUTOMATISCH);
				screeningRondeGebeurtenis.setDatum(herzieningOntvangen);
				screeningRondeGebeurtenis.setVerslag(cytologieVerslag);
				rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
			}
		}
	}

	private boolean heeftStatusOntvangenGehadVoor(List uitstrijkjeHistory, Date statusDatum)
	{
		for (int i = uitstrijkjeHistory.size() - 1; i >= 0; i--) 
		{
			CervixUitstrijkje auditUitstrijkje = EntityAuditUtil.getRevisionEntity(uitstrijkjeHistory.get(i));
			if (auditUitstrijkje.getStatusDatum().after(statusDatum))
			{
				return false; 
			}
			else if (CervixUitstrijkjeStatus.ONTVANGEN == auditUitstrijkje.getUitstrijkjeStatus())
			{
				return true; 
			}
		}
		return false; 
	}

	private void addRondeGebeurtenisToDossier(ScreeningRondeGebeurtenissen rondeDossier, InpakbareUitnodiging<?> uitnodiging, ScreenitRevisionEntity revisionEntity,
		ScreeningRondeGebeurtenis screeningRondeGebeurtenis, List<String> extraOmschrijvingen, TypeGebeurtenis gebeurtenis)
	{
		if (gebeurtenis != null)
		{
			screeningRondeGebeurtenis.setGebeurtenis(gebeurtenis);
			screeningRondeGebeurtenis.setUitnodiging(uitnodiging);
			screeningRondeGebeurtenis.setExtraOmschrijving(extraOmschrijvingen.toArray(new String[] {}));
			screeningRondeGebeurtenis
				.setBron(revisionEntity.getGebruiker() != null || revisionEntity.getInstellingGebruiker() != null ? GebeurtenisBron.MEDEWERKER : GebeurtenisBron.AUTOMATISCH);
			rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
		}
	}

	private void addLabformulierStatusGebeurtenissen(ScreeningRondeGebeurtenissen rondeDossier, CervixLabformulier labformulier)
	{
		if (labformulier.getStatus() != CervixLabformulierStatus.GESCAND && labformulier.getStatus() != CervixLabformulierStatus.AFGEKEURD)
		{
			ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
			screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.BMHK_LABFORMULIER_GECONTROLEERD);
			screeningRondeGebeurtenis.setUitnodiging(labformulier.getUitstrijkje().getUitnodiging());
			List<String> extraOmschrijvingen = new ArrayList<>();
			extraOmschrijvingen.add("Monster-id: " + labformulier.getUitstrijkje().getMonsterId());
			extraOmschrijvingen.add("Status: ");
			extraOmschrijvingen.add(EnumStringUtil.getPropertyString(labformulier.getStatus()));
			screeningRondeGebeurtenis.setExtraOmschrijving(extraOmschrijvingen.toArray(new String[] {}));
			screeningRondeGebeurtenis.setDatum(labformulier.getStatusDatum());
			screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(labformulier));
			rondeDossier.addGebeurtenis(screeningRondeGebeurtenis);
		}
	}

	private void addZasStatusGebeurtenissen(ScreeningRondeGebeurtenissen rondeDossier, CervixZas zas)
	{
		Map<CervixZasStatus, Object> alleLaatsteStatussen = new EnumMap<>(CervixZasStatus.class);

		for (Object auditRow : EntityAuditUtil.getEntityHistory(zas, hibernateService.getHibernateSession(), false))
		{
			CervixZasStatus status = ((CervixZas) EntityAuditUtil.getRevisionEntity(auditRow)).getZasStatus();
			long timestamp = EntityAuditUtil.getRevisionInfo(auditRow).getTimestamp();
			boolean toevoegen = true;
			for (Entry<CervixZasStatus, Object> newestAuditRow : alleLaatsteStatussen.entrySet())
			{
				CervixZasStatus status1 = newestAuditRow.getKey();
				long timestamp1 = EntityAuditUtil.getRevisionInfo(newestAuditRow.getValue()).getTimestamp();
				if (status.equals(status1) && timestamp1 > timestamp)
				{
					toevoegen = false;
					break;
				}
			}
			if (toevoegen)
			{
				alleLaatsteStatussen.put(status, auditRow);
			}
		}
		CervixZasStatus currentStatus = null;

		for (Object object : alleLaatsteStatussen.values())
		{
			CervixZas auditedZas = EntityAuditUtil.getRevisionEntity(object);
			if (currentStatus == null || !auditedZas.getZasStatus().equals(currentStatus))
			{
				ScreenitRevisionEntity revisionEntity = EntityAuditUtil.getRevisionInfo(object);
				ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
				List<String> extraOmschrijvingen = new ArrayList<>();
				extraOmschrijvingen.add("Monster-id: " + zas.getMonsterId());
				TypeGebeurtenis gebeurtenis = null;
				screeningRondeGebeurtenis.setDatum(auditedZas.getStatusDatum());
				switch (auditedZas.getZasStatus()) 
				{
				case ONTVANGEN:
					gebeurtenis = TypeGebeurtenis.BMHK_MONSTER_ONTVANGEN;
					break;
				case NIET_ANALYSEERBAAR:
					gebeurtenis = TypeGebeurtenis.BMHK_MONSTER_NIET_ANALYSEERBAAR;
					break;
				default:
					break;
				}
				addRondeGebeurtenisToDossier(rondeDossier, zas.getUitnodiging(), revisionEntity, screeningRondeGebeurtenis, extraOmschrijvingen, gebeurtenis);
			}
			currentStatus = auditedZas.getZasStatus();
		}
	}

	@Override
	public GebeurtenisBron bepaalGebeurtenisBron(HibernateObject entity)
	{
		return bepaalGebeurtenisBron(entity, null);
	}

	@Override
	public GebeurtenisBron bepaalGebeurtenisBron(HibernateObject entity, AuditCriterion extraCriteria)
	{
		return bepaalGebeurtenisBron(entity, extraCriteria, true);
	}

	private GebeurtenisBron bepaalGebeurtenisBron(HibernateObject entity, boolean max)
	{
		return bepaalGebeurtenisBron(entity, null, max);
	}

	private GebeurtenisBron bepaalGebeurtenisBron(HibernateObject entity, AuditCriterion extraCriteria, boolean max)
	{
		GebeurtenisBron bron;
		AuditQuery query = EntityAuditUtil.createQuery(entity, hibernateService.getHibernateSession());
		if (max)
		{
			query.addProjection(AuditEntity.revisionNumber().max());
		}
		else
		{
			query.addProjection(AuditEntity.revisionNumber().min());
		}
		if (extraCriteria != null)
		{
			query.add(extraCriteria);
		}
		Number lastRevision = (Number) query.getSingleResult();
		ScreenitRevisionEntity revisionEntity = null;
		if (lastRevision != null)
		{
			revisionEntity = hibernateService.get(ScreenitRevisionEntity.class, lastRevision);
		}
		bron = dossierAuditService.getGebeurtenisBron(revisionEntity);
		return bron;
	}

	private Date getClientAfnameDatum(IFOBTTest entity)
	{
		AuditQuery query = EntityAuditUtil.createQuery(entity, hibernateService.getHibernateSession());
		query.add(AuditEntity.revisionProperty("client").isNotNull());
		query.addOrder(AuditEntity.revisionNumber().desc());
		List<?> objecten = query.getResultList();
		Date afnamedatum = null;
		if (!objecten.isEmpty())
		{
			IFOBTTest result = EntityAuditUtil.getRevisionEntity(objecten.get(0));
			afnamedatum = result.getAfnameDatum();
		}
		return afnamedatum;
	}

	private void filterClientContacten(List<Bevolkingsonderzoek> bevolkingsonderzoeken, List<ClientContact> clientContacten)
	{
		if (bevolkingsonderzoeken != null)
		{
			Iterator<ClientContact> i = clientContacten.iterator();
			while (i.hasNext())
			{
				ClientContact clientContact = i.next();
				boolean filterClientContact = true;
				for (ClientContactActie clientContactActie : clientContact.getActies())
				{
					if (!Collections.disjoint(clientContactActie.getType().getBevolkingsonderzoeken(), bevolkingsonderzoeken))
					{
						filterClientContact = false;
						break;
					}
				}
				if (filterClientContact)
				{
					i.remove();
				}
			}
		}
	}

	@Override
	public Iterator<ClientContact> getClientContacten(Client client, List<Bevolkingsonderzoek> bevolkingsonderzoeken, long first, long count, String sortProperty,
		boolean ascending)
	{
		List<ClientContact> clientContacten = clientDao.getClientContacten(client, -1, -1, sortProperty, ascending);
		filterClientContacten(bevolkingsonderzoeken, clientContacten);

		return clientContacten.subList(Ints.checkedCast(first), Ints.checkedCast(first + count)).iterator();
	}

	@Override
	public int countClientContacten(Client client, List<Bevolkingsonderzoek> bevolkingsonderzoeken)
	{
		List<ClientContact> clientContacten = clientDao.getClientContacten(client);
		filterClientContacten(bevolkingsonderzoeken, clientContacten);

		return clientContacten.size();
	}

	@Override
	public List<DossierGebeurtenis> getColonDossierGebeurtenissen(Client client)
	{
		List<DossierGebeurtenis> gebeurtenissen = new ArrayList<>();

		ColonDossier colonDossier = client.getColonDossier();

		List<ColonAfmelding> colonAfmeldingen = new ArrayList<>();
		if (!colonDossier.getAfmeldingen().isEmpty())
		{
			colonAfmeldingen.addAll(colonDossier.getAfmeldingen());
		}
		if (!colonDossier.getScreeningRondes().isEmpty())
		{
			var colonTijdelijkeAfmeldingen = colonDossier.getScreeningRondes().stream()
				.flatMap(screeningRonde -> screeningRonde.getAfmeldingen().stream())
				.filter(afmelding -> AfmeldingType.TIJDELIJK.equals(afmelding.getType()))
				.collect(Collectors.toList());
			colonAfmeldingen.addAll(colonTijdelijkeAfmeldingen);
		}

		for (ColonAfmelding afmelding : colonAfmeldingen)
		{
			if (afmelding != null && (AfmeldingType.DEFINITIEF.equals(afmelding.getType()) || AfmeldingType.TIJDELIJK.equals(afmelding.getType())))
			{
				if (afmelding.getAfmeldingStatus() != null && afmelding.getStatusAfmeldDatum() != null)
				{

					AfmeldenDossierGebeurtenis<ColonAfmelding> dossierGebeurtenis = new AfmeldenDossierGebeurtenis<>(afmelding.getStatusAfmeldDatum());
					dossierGebeurtenis.setDossierGebeurtenisType(DossierGebeurtenisType.AFMELDING);
					dossierGebeurtenis.setAfmelding(afmelding);
					dossierGebeurtenis.setBron(bepaalGebeurtenisBron(afmelding,
						AuditEntity.and(AuditEntity.property("heraanmeldStatus").isNull(), AuditEntity.property("statusHeraanmeldDatum").isNull())));
					gebeurtenissen.add(dossierGebeurtenis);
				}
				if (afmelding.getHeraanmeldStatus() != null && afmelding.getStatusHeraanmeldDatum() != null && afmelding.getAfmeldingStatus() != AanvraagBriefStatus.BRIEF)
				{

					AfmeldenDossierGebeurtenis<ColonAfmelding> dossierGebeurtenis = new AfmeldenDossierGebeurtenis<>(afmelding.getStatusHeraanmeldDatum());
					dossierGebeurtenis.setDossierGebeurtenisType(DossierGebeurtenisType.HERAANMELDING);
					dossierGebeurtenis.setHeraanmelding(afmelding);
					dossierGebeurtenis.setBron(bepaalGebeurtenisBron(afmelding));
					gebeurtenissen.add(dossierGebeurtenis);
				}
			}
		}
		if (colonDossier.getLaatsteScreeningRonde() != null && colonDossier.getLaatsteScreeningRonde().getOpenUitnodiging() != null)
		{
			OpenUitnodiging ou = colonDossier.getLaatsteScreeningRonde().getOpenUitnodiging();
			if (OpenUitnodigingUitslag.CLIENT_ONDER_CONTROLE.equals(ou.getUitslag()) || OpenUitnodigingUitslag.CLIENT_OVER_10_JAAR_UITNODIGINGEN.equals(ou.getUitslag()))
			{
				OpenUitnodigingDossierGebeurtenis dossierGebeurtenis = new OpenUitnodigingDossierGebeurtenis(colonDossier.getInactiefVanaf());
				dossierGebeurtenis.setDossierGebeurtenisType(DossierGebeurtenisType.OPEN_UITNODIGING);
				dossierGebeurtenis.setBron(GebeurtenisBron.MEDEWERKER);

				AuditQuery query = EntityAuditUtil.createQuery(ou, hibernateService.getHibernateSession());
				query.add(AuditEntity.property("uitslag").eq(OpenUitnodigingUitslag.CLIENT_ONDER_CONTROLE));
				query.add(AuditEntity.revisionType().eq(RevisionType.MOD));

				List clientOnderControleAudit = query.getResultList();
				if (clientOnderControleAudit != null && !clientOnderControleAudit.isEmpty())
				{

					Object auditRow = clientOnderControleAudit.get(0);
					long timestamp = EntityAuditUtil.getRevisionInfo(auditRow).getTimestamp();
					dossierGebeurtenis.setTijd(new Date(timestamp));
				}
				else
				{
					LOG.error("Audit timestamp niet gevonden bij open uitnodiging. uitnodiging-id: " + ou.getId());
					dossierGebeurtenis.setTijd(ou.getDatum());
				}

				dossierGebeurtenis.setOpenUitnodiging(ou);
				gebeurtenissen.add(dossierGebeurtenis);
			}
		}
		return gebeurtenissen;
	}

	@Override
	public List<DossierGebeurtenis> getCervixDossierGebeurtenissen(Client client)
	{
		List<DossierGebeurtenis> gebeurtenissen = new ArrayList<>();

		if (client.getCervixDossier() != null)
		{
			CervixDossier cervixDossier = client.getCervixDossier();
			for (CervixAfmelding cervixAfmelding : cervixDossier.getAfmeldingen())
			{
				if (cervixAfmelding != null)
				{
					if (cervixAfmelding.getAfmeldingStatus() != null && cervixAfmelding.getStatusAfmeldDatum() != null
						&& AfmeldingType.DEFINITIEF.equals(cervixAfmelding.getType()))
					{

						AfmeldenDossierGebeurtenis<CervixAfmelding> dossierGebeurtenis = new AfmeldenDossierGebeurtenis<>(cervixAfmelding.getStatusAfmeldDatum());
						dossierGebeurtenis.setDossierGebeurtenisType(DossierGebeurtenisType.AFMELDING);
						dossierGebeurtenis.setAfmelding(cervixAfmelding);
						dossierGebeurtenis.setBron(bepaalGebeurtenisBron(cervixAfmelding,
							AuditEntity.and(AuditEntity.property("heraanmeldStatus").isNull(), AuditEntity.property("statusHeraanmeldDatum").isNull())));
						gebeurtenissen.add(dossierGebeurtenis);
					}
					if (cervixAfmelding.getHeraanmeldStatus() != null && cervixAfmelding.getStatusHeraanmeldDatum() != null)
					{

						AfmeldenDossierGebeurtenis<CervixAfmelding> dossierGebeurtenis = new AfmeldenDossierGebeurtenis<>(
							cervixAfmelding.getStatusHeraanmeldDatum());
						dossierGebeurtenis.setDossierGebeurtenisType(DossierGebeurtenisType.HERAANMELDING);
						dossierGebeurtenis.setHeraanmelding(cervixAfmelding);
						dossierGebeurtenis.setBron(bepaalGebeurtenisBron(cervixAfmelding));
						gebeurtenissen.add(dossierGebeurtenis);
					}
				}
			}
		}
		return gebeurtenissen;
	}

	@Override
	public List<DossierGebeurtenis> getMammaDossierGebeurtenissen(Client client)
	{
		List<DossierGebeurtenis> gebeurtenissen = new ArrayList<>();

		if (client.getMammaDossier() != null)
		{
			MammaDossier dossier = client.getMammaDossier();
			for (MammaAfmelding afmelding : dossier.getAfmeldingen())
			{
				if (afmelding != null)
				{
					if (afmelding.getAfmeldingStatus() != null && afmelding.getStatusAfmeldDatum() != null
						&& AfmeldingType.DEFINITIEF.equals(afmelding.getType()))
					{

						AfmeldenDossierGebeurtenis<MammaAfmelding> dossierGebeurtenis = new AfmeldenDossierGebeurtenis<>(afmelding.getStatusAfmeldDatum());
						dossierGebeurtenis.setDossierGebeurtenisType(DossierGebeurtenisType.AFMELDING);
						dossierGebeurtenis.setAfmelding(afmelding);
						dossierGebeurtenis.setBron(bepaalGebeurtenisBron(afmelding,
							AuditEntity.and(AuditEntity.property("heraanmeldStatus").isNull(), AuditEntity.property("statusHeraanmeldDatum").isNull())));
						gebeurtenissen.add(dossierGebeurtenis);
					}
					if (afmelding.getHeraanmeldStatus() != null && afmelding.getStatusHeraanmeldDatum() != null)
					{

						AfmeldenDossierGebeurtenis<MammaAfmelding> dossierGebeurtenis = new AfmeldenDossierGebeurtenis<>(
							afmelding.getStatusHeraanmeldDatum());
						dossierGebeurtenis.setDossierGebeurtenisType(DossierGebeurtenisType.HERAANMELDING);
						dossierGebeurtenis.setHeraanmelding(afmelding);
						dossierGebeurtenis.setBron(bepaalGebeurtenisBron(afmelding));
						gebeurtenissen.add(dossierGebeurtenis);
					}
				}
			}
		}
		return gebeurtenissen;
	}

	@Override
	public List<ScreeningRondeGebeurtenis> getProjectGebeurtenissen(ProjectClient pClient)
	{
		List<ScreeningRondeGebeurtenis> gebeurtenissen = new ArrayList<>();
		gebeurtenissen.addAll(getProjectBrievenGebeurtenissen(pClient.getBrieven()));
		return gebeurtenissen;
	}

	private List<ScreeningRondeGebeurtenis> getProjectBrievenGebeurtenissen(List<ProjectBrief> projectBrieven)
	{
		List<ScreeningRondeGebeurtenis> gebeurtenissen = new ArrayList<>();
		for (ProjectBrief projectBrief : projectBrieven)
		{
			ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
			screeningRondeGebeurtenis.setBrief(projectBrief);
			getProjectDefinitieExtraOmschrijving(screeningRondeGebeurtenis, projectBrief);
			if (projectBrief.isGegenereerd())
			{
				MergedBrieven<?> mergedBrieven = projectBrief.getMergedBrieven();
				screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(mergedBrieven));
				if (mergedBrieven == null || mergedBrieven.getPrintDatum() != null)
				{
					if (mergedBrieven != null)
					{
						screeningRondeGebeurtenis.setDatum(mergedBrieven.getPrintDatum());
					}
					else
					{
						screeningRondeGebeurtenis.setDatum(projectBrief.getCreatieDatum());
					}
					screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.PROJECT_BRIEF_AFGEDRUKT);

					ScreeningRondeGebeurtenis vragenlijstGebeurtenis = getVragenlijstGebeurtenis(projectBrief);
					if (vragenlijstGebeurtenis != null)
					{
						gebeurtenissen.add(vragenlijstGebeurtenis);
					}
				}
				else
				{
					screeningRondeGebeurtenis.setDatum(mergedBrieven.getCreatieDatum());
					screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.PROJECT_BRIEF_KLAARGEZET);
				}
			}
			else if (projectBrief.isVervangen())
			{
				screeningRondeGebeurtenis.setDatum(projectBrief.getCreatieDatum());
				screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.PROJECT_BRIEF_VERVANGEN);
				screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(projectBrief));
			}
			else if (projectBrief.isTegenhouden())
			{
				screeningRondeGebeurtenis.setDatum(projectBrief.getCreatieDatum());
				screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.PROJECT_BRIEF_TEGENHOUDEN);
				screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(projectBrief));
			}
			else
			{
				screeningRondeGebeurtenis.setDatum(projectBrief.getCreatieDatum());
				screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.PROJECT_BRIEF_AANGEMAAKT);
				screeningRondeGebeurtenis.setBron(bepaalGebeurtenisBron(projectBrief));
			}
			gebeurtenissen.add(screeningRondeGebeurtenis);
		}
		return gebeurtenissen;
	}

	@Override
	public List<ScreeningRondeGebeurtenis> getAlgemeneBriefGebeurtenissen(List<AlgemeneBrief> brieven)
	{
		List<ScreeningRondeGebeurtenis> gebeurtenissen = new ArrayList<>();
		brieven.forEach(brief ->
		{
			ClientBrief herdruk = BriefUtil.getHerdruk(brief);
			ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
			List<String> extraOmschrijvingen = new ArrayList<>();
			maakScreeningRondeGebeurtenisBrief(brief, herdruk, screeningRondeGebeurtenis, extraOmschrijvingen);
			screeningRondeGebeurtenis.setExtraOmschrijving(extraOmschrijvingen.toArray(new String[] {}));
			gebeurtenissen.add(screeningRondeGebeurtenis);
		});
		return gebeurtenissen;
	}

	private ScreeningRondeGebeurtenis getVragenlijstGebeurtenis(ProjectBrief projectBrief)
	{
		ProjectVragenlijstAntwoordenHolder holder = projectBrief.getVragenlijstAntwoordenHolder();
		if (holder != null)
		{
			if (ProjectVragenlijstStatus.AFGEROND.equals(holder.getStatus()))
			{
				ScreeningRondeGebeurtenis screeningRondeGebeurtenis = new ScreeningRondeGebeurtenis();
				screeningRondeGebeurtenis.setBrief(projectBrief);

				ScannedVragenlijst scannedVragenlijst = holder.getScannedVragenlijst();
				if (scannedVragenlijst != null)
				{
					screeningRondeGebeurtenis.setDatum(holder.getScannedVragenlijst().getScanDatum());
					screeningRondeGebeurtenis.setBron(GebeurtenisBron.AUTOMATISCH);
					if (ScannedVragenlijst.STATUS_AFGEHANDELD.equals(scannedVragenlijst.getStatus()))
					{
						screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.PROJECT_VRAGENLIJST_ONTVANGEN_PAPIER);
					}
					else if (ScannedVragenlijst.STATUS_VERWIJDERD.equals(scannedVragenlijst.getStatus()))
					{
						screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.PROJECT_VRAGENLIJST_ONTVANGEN_PAPIER_VERWIJDERD);
					}
				}
				else
				{
					screeningRondeGebeurtenis.setDatum(holder.getLaatstGewijzigd());
					screeningRondeGebeurtenis.setBron(GebeurtenisBron.CLIENT);
					screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.PROJECT_VRAGENLIJST_ONTVANGEN_DIGITAAL);
				}
				return screeningRondeGebeurtenis;
			}
		}
		return null;
	}

	private ScreeningRondeGebeurtenis getProjectDefinitieExtraOmschrijving(ScreeningRondeGebeurtenis screeningRondeGebeurtenis, ProjectBrief brief)
	{
		String actieType = "";
		String briefType = "";
		ProjectBriefActie actie = brief.getDefinitie();
		String documentName = actie.getDocument().getNaam();
		if (actie.getType() != null)
		{
			actieType = EnumStringUtil.getPropertyString(actie.getType());
			if ((ProjectBriefActieType.VERVANGENDEBRIEF.equals(actie.getType()) || ProjectBriefActieType.XDAGENNAY.equals(actie.getType())
				|| ProjectBriefActieType.XMETY.equals(actie.getType())) && actie.getBriefType() != null)
			{
				briefType = EnumStringUtil.getPropertyString(actie.getBriefType());
			}
		}

		if (!actieType.isEmpty() && briefType.isEmpty() && !documentName.isEmpty())
		{
			screeningRondeGebeurtenis.setExtraOmschrijving(actieType, documentName);
		}
		else if (!actieType.isEmpty() && !briefType.isEmpty() && !documentName.isEmpty())
		{
			screeningRondeGebeurtenis.setExtraOmschrijving(actieType, briefType, documentName);
		}
		else if (actieType.isEmpty() && !briefType.isEmpty() && !documentName.isEmpty())
		{
			screeningRondeGebeurtenis.setExtraOmschrijving(documentName);
		}
		return screeningRondeGebeurtenis;
	}

}
