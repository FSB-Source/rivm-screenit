package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.batch.dao.VerwerkHpvBerichtenDao;
import nl.rivm.screenit.batch.model.HapiContextType;
import nl.rivm.screenit.batch.service.CervixBepaalHpvBeoordelingService;
import nl.rivm.screenit.batch.service.CervixVerwerkHpvBerichtService;
import nl.rivm.screenit.dao.cervix.CervixMonsterDao;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.cervix.CervixHpvBeoordeling;
import nl.rivm.screenit.model.cervix.CervixHpvBericht;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvBerichtWrapper;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvMonsterWrapper;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.enums.CervixZasStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.service.cervix.CervixVervolgService;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import ca.uhn.hl7v2.HapiContext;
import ca.uhn.hl7v2.model.Message;
import ca.uhn.hl7v2.model.v251.message.OUL_R22;
import ca.uhn.hl7v2.parser.Parser;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixVerwerkHpvBerichtServiceImpl implements CervixVerwerkHpvBerichtService
{

	private static final Logger LOG = LoggerFactory.getLogger(CervixVerwerkHpvBerichtServiceImpl.class);

	@Autowired
	private CervixMonsterDao monsterDao;

	@Autowired
	private VerwerkHpvBerichtenDao verwerkHpvBerichtenDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private LogService logService;

	@Autowired
	private CervixFactory factory;

	@Autowired
	private CervixBepaalHpvBeoordelingService bepaalHpvBeoordelingService;

	@Autowired
	private CervixVervolgService vervolgService;

	private boolean isGenotyperingAnalyseGestart;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public List<CervixHpvBericht> getAlleNietVerwerkteHpvBerichten()
	{
		return verwerkHpvBerichtenDao.getAlleNietVerwerkteHpvBerichten();
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwerkOntvangenHpvBericht(Long berichtId)
	{
		CervixHpvBericht ontvangenBericht = hibernateService.get(CervixHpvBericht.class, berichtId);

		List<Instelling> opDashboardVanOrganisaties = Arrays.asList(ontvangenBericht.getLaboratorium());
		try
		{
			String hpvBericht = ontvangenBericht.getHl7Bericht();
			HapiContext context = ScreenitHapiContext.getHapiContext(HapiContextType.UTF_8);
			Parser p = context.getPipeParser();
			Message hapiMsg = p.parse(hpvBericht);
			CervixHpvBerichtWrapper bericht = new CervixHpvBerichtWrapper((OUL_R22) hapiMsg);

			LOG.info("Bericht (" + bericht.getMessageId() + ") wordt verwerkt! Aantal: " + bericht.getResultaten().size());
			for (CervixHpvMonsterWrapper sample : bericht.getResultaten())
			{
				try
				{
					CervixMonster monster = validateBarcode(sample, opDashboardVanOrganisaties);
					validatieAnalyseDatumEnAutorisatieDatum(monster, sample, opDashboardVanOrganisaties);
					validateStatusMonster(monster, sample, opDashboardVanOrganisaties);

					CervixHpvBeoordelingWaarde beoordelingWaarde = bepaalHpvBeoordelingService.getHpvBeoordelingWaarde(sample.getAnalyseresultaten());
					if (beoordelingWaarde != null)
					{
						valideerMonsterMagOverschrevenWorden(sample, monster, opDashboardVanOrganisaties);
						CervixScreeningRonde ronde = monster.getOntvangstScreeningRonde();
						factory.maakHpvBeoordeling(monster, ontvangenBericht, sample.getAnalyseDatum(), sample.getAutorisatieDatum(), beoordelingWaarde,
							sample.getAnalyseresultaten());

						setMonsterInVolgendeStatus(monster);
						hibernateService.saveOrUpdate(monster);

						if (CervixHpvBeoordelingWaarde.ONGELDIG != beoordelingWaarde)
						{
							ronde.setMonsterHpvUitslag(monster);
							hibernateService.saveOrUpdate(ronde);
						}
						else if (monster.getHpvBeoordelingen().size() == 1)
						{
							String melding = "Eerste ongeldige hrHPV-analyseresultaat(en) van monster. Monster dient nogmaals op HPV beoordeeld te worden.";
							logging(LogGebeurtenis.CERVIX_HPV_UITSLAG_VERWERKT, opDashboardVanOrganisaties, Level.INFO, melding, sample, monster);
						}
						vervolgService.digitaalLabformulierKlaarVoorCytologie(monster);
					}
					else
					{
						Level onverwachteBerichtWaardeLogLevel = Level.ERROR;
						String onverwachteBerichtWaardeMeldingPrefix = "Onverwachte ";
						if (sample.isFailure())
						{
							onverwachteBerichtWaardeLogLevel = Level.WARNING; 
							onverwachteBerichtWaardeMeldingPrefix = "";
						}
						String melding = onverwachteBerichtWaardeMeldingPrefix + "hrHPV-analyseresultaat(en) (" + sample.getAnalyseresultatenString()
							+ ") van monster ontvangen. Aanlevering wordt genegeerd.";
						melding = logging(LogGebeurtenis.CERVIX_HPV_UITSLAG_GENEGEERD, opDashboardVanOrganisaties, onverwachteBerichtWaardeLogLevel, melding, sample, monster);
						throw new IllegalStateException(melding);
					}

				}
				catch (IllegalStateException e)
				{

					LOG.error(e.getMessage(), e);
					continue;
				}
			}
		}
		catch (Exception e)
		{
			LOG.error("Er is een fout opgetreden met het verwerken van bericht " + ontvangenBericht.getMessageId(), e);
			throw new IllegalStateException("Bericht kon niet worden uitgelezen");
		}
		ontvangenBericht.setStatus(BerichtStatus.VERWERKT);
		ontvangenBericht.setStatusDatum(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(ontvangenBericht);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void logError(Long berichtId, String message)
	{
		CervixHpvBericht ontvangenBericht = hibernateService.get(CervixHpvBericht.class, berichtId);
		ontvangenBericht.setStatus(BerichtStatus.FOUT);
		hibernateService.saveOrUpdate(ontvangenBericht);
		BMHKLaboratorium laboratorium = ontvangenBericht.getLaboratorium();
		List<Instelling> opDashboardVanOrganisaties = Arrays.asList(laboratorium);
		logging(LogGebeurtenis.CERVIX_HPV_BERICHT_VERWERKT, opDashboardVanOrganisaties, Level.ERROR,
			"Bericht (messageID: " + ontvangenBericht.getMessageId() + ") kon niet worden verwerkt. " + (message != null ? "(" + message + ")" : ""),
			null);
	}

	private String logging(LogGebeurtenis gebeurtenis, List<Instelling> lab, Level level, String melding, CervixHpvMonsterWrapper sample)
	{
		return logging(gebeurtenis, lab, level, melding, sample, null);
	}

	private String logging(LogGebeurtenis gebeurtenis, List<Instelling> instellingen, Level level, String melding, CervixHpvMonsterWrapper sample, CervixMonster monster)
	{
		if (sample != null)
		{
			String sampleGegevens = " Monster-id: " + sample.getBarcode() + ".";
			melding += sampleGegevens;
		}

		for (Instelling lab : instellingen)
		{
			if (lab.getOrganisatieType() == OrganisatieType.BMHK_LABORATORIUM)
			{
				melding += " Laboratorium: " + lab.getNaam();
				break;
			}
		}

		Client client = null;
		if (monster != null && monster.getUitnodiging() != null && monster.getUitnodiging().getScreeningRonde() != null
			&& monster.getUitnodiging().getScreeningRonde().getDossier() != null)
		{
			client = monster.getUitnodiging().getScreeningRonde().getDossier().getClient();
		}

		LogEvent event = new LogEvent();
		event.setLevel(level);
		event.setMelding(melding);
		if (client != null)
		{
			logService.logGebeurtenis(gebeurtenis, instellingen, event, client, Bevolkingsonderzoek.CERVIX);
		}
		else
		{
			logService.logGebeurtenis(gebeurtenis, instellingen, event, Bevolkingsonderzoek.CERVIX);
		}
		return melding;
	}

	private void setMonsterInVolgendeStatus(CervixMonster monster)
	{
		monster = (CervixMonster) HibernateHelper.deproxy(monster);
		if (monster instanceof CervixUitstrijkje)
		{
			CervixUitstrijkje uitstrijkje = CervixMonsterUtil.getUitstrijkje(monster);
			if (CervixUitstrijkjeStatus.ONTVANGEN == uitstrijkje.getUitstrijkjeStatus())
			{
				uitstrijkje.setUitstrijkjeStatus(CervixUitstrijkjeStatus.GEANALYSEERD_OP_HPV_POGING_1);
			}
			else if (CervixUitstrijkjeStatus.GEANALYSEERD_OP_HPV_POGING_1 == uitstrijkje.getUitstrijkjeStatus())
			{
				uitstrijkje.setUitstrijkjeStatus(CervixUitstrijkjeStatus.GEANALYSEERD_OP_HPV_POGING_2);
			}
		}
		else
		{
			CervixZas zas = CervixMonsterUtil.getZAS(monster);
			if (CervixZasStatus.ONTVANGEN == zas.getZasStatus())
			{
				zas.setZasStatus(CervixZasStatus.GEANALYSEERD_OP_HPV_POGING_1);
			}
			else if (CervixZasStatus.GEANALYSEERD_OP_HPV_POGING_1 == zas.getZasStatus())
			{
				zas.setZasStatus(CervixZasStatus.GEANALYSEERD_OP_HPV_POGING_2);
			}
		}
		monster.setStatusDatum(currentDateSupplier.getDate());
	}

	private void validatieAnalyseDatumEnAutorisatieDatum(CervixMonster monster, CervixHpvMonsterWrapper sample, List<Instelling> opDashboardVanOrganisaties)
	{
		SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy hh:mm:ss");
		Date analyseDatum = sample.getAnalyseDatum();
		for (CervixHpvBeoordeling hpvBeoordeling : monster.getHpvBeoordelingen())
		{
			if (format.format(hpvBeoordeling.getAnalyseDatum()).equals(format.format(analyseDatum)))
			{
				String melding = "hrHPV-analyseresultaat(en) zijn dubbel ontvangen. Aanlevering wordt genegeerd.";
				melding = logging(LogGebeurtenis.CERVIX_HPV_UITSLAG_GENEGEERD, opDashboardVanOrganisaties, Level.WARNING, melding, sample, monster);
				throw new IllegalStateException(melding);
			}
		}
	}

	private boolean isErAlEenEerdereInvalidUitslag(CervixMonster monster)
	{
		List<CervixHpvBeoordeling> hpvBeoordelingen = monster.getHpvBeoordelingen();
		if (hpvBeoordelingen.size() == 1)
		{
			CervixHpvBeoordeling beoordeling = hpvBeoordelingen.get(0);
			if (CervixHpvBeoordelingWaarde.ONGELDIG == beoordeling.getHpvUitslag())
			{
				return true;
			}
		}
		return false;
	}

	private void valideerMonsterMagOverschrevenWorden(CervixHpvMonsterWrapper sample, CervixMonster monster, List<Instelling> opDashboardVanOrganisaties)
	{
		CervixScreeningRonde ronde = monster.getOntvangstScreeningRonde();
		boolean eerdereHPVuitslag = ronde.getMonsterHpvUitslag() != null;
		if (!eerdereHPVuitslag)
		{
			return;
		}
		boolean eerderHpvUitslagIsZas = eerdereHPVuitslag && CervixMonsterUtil.isZAS(ronde.getMonsterHpvUitslag());
		boolean isCommunicatieUitgestuurd = ronde.getMonsterHpvUitslag().getBrief() != null;
		if (eerderHpvUitslagIsZas && !isCommunicatieUitgestuurd)
		{
			CervixHpvBeoordeling beoordeling = ronde.getMonsterHpvUitslag().getLaatsteHpvBeoordeling();
			String oudeBeoordeling = beoordeling.getHpvUitslag().getNaam();
			String melding = "hrHPV-analyseresultaat(en) van uitstrijkje overschrijft hrHPV-analyseresultaat(en) van ZAS, status veranderd van " + oudeBeoordeling + " naar "
				+ bepaalHpvBeoordelingService.getHpvBeoordelingWaarde(sample.getAnalyseresultaten()).getNaam() + ".";
			logging(LogGebeurtenis.CERVIX_HPV_UITSLAG_OVERSCHREVEN, opDashboardVanOrganisaties, Level.INFO, melding, sample, monster);
			return;
		}
		String melding = "hrHPV-analyseresultaat(en) in een ronde, waar al een hrHPV-analyseresultaat(en) bestaat. Aanlevering wordt genegeerd.";
		melding = logging(LogGebeurtenis.CERVIX_HPV_UITSLAG_GENEGEERD, opDashboardVanOrganisaties, Level.WARNING, melding, sample, monster);
		throw new IllegalStateException(melding);
	}

	private CervixMonster validateBarcode(CervixHpvMonsterWrapper sample, List<Instelling> opDashboardVanOrganisaties) throws IllegalStateException
	{
		String barcode = sample.getBarcode();
		CervixMonster monster = monsterDao.getMonsterByMonsterId(barcode);
		if (monster != null)
		{
			return monster;
		}
		boolean isVerwijderdeMonster = monsterDao.isVerwijderdMonster(barcode);
		String melding = String.format("hrHPV-analyseresultaat(en) van onbekend monster%s. Aanlevering wordt genegeerd.",
			isVerwijderdeMonster ? " (clientdossier verwijderd)" : "");
		melding = logging(LogGebeurtenis.CERVIX_HPV_ONBEKENDE_BARCODE, opDashboardVanOrganisaties, Level.WARNING, melding, sample);
		throw new IllegalStateException(melding);
	}

	private void validateStatusMonster(CervixMonster monster, CervixHpvMonsterWrapper sample, List<Instelling> opDashboardVanOrganisaties) throws IllegalStateException
	{
		String status = null;
		String melding = "";
		if (CervixMonsterUtil.isUitstrijkje(monster))
		{
			CervixUitstrijkje uitstrijkje = CervixMonsterUtil.getUitstrijkje(monster);
			if (CervixUitstrijkjeStatus.ONTVANGEN == uitstrijkje.getUitstrijkjeStatus()
				|| CervixUitstrijkjeStatus.GEANALYSEERD_OP_HPV_POGING_1 == uitstrijkje.getUitstrijkjeStatus() && isErAlEenEerdereInvalidUitslag(monster))
			{
				return;
			}
			status = uitstrijkje.getUitstrijkjeStatus().getNaam();
			melding = "hrHPV-analyseresultaat(en) van uitstrijkje met onverwachte status: " + status + ". Aanlevering wordt genegeerd.";
		}

		if (CervixMonsterUtil.isZAS(monster))
		{
			CervixZas zas = CervixMonsterUtil.getZAS(monster);
			if (CervixZasStatus.ONTVANGEN == zas.getZasStatus() || CervixZasStatus.GEANALYSEERD_OP_HPV_POGING_1 == zas.getZasStatus() && isErAlEenEerdereInvalidUitslag(monster))
			{
				return;
			}
			status = zas.getZasStatus().getNaam();
			melding = "hrHPV-analyseresultaat(en) van ZAS met onverwachte status: " + status + ". Aanlevering wordt genegeerd.";
		}

		melding = logging(LogGebeurtenis.CERVIX_HPV_UITSLAG_GENEGEERD, opDashboardVanOrganisaties, Level.WARNING, melding, sample, monster);
		throw new IllegalStateException(melding);
	}
}
