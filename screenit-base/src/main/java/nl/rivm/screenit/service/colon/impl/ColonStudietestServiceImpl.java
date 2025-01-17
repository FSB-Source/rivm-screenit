package nl.rivm.screenit.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.ProjectParameterKey;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.enums.IFOBTBestandStatus;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.project.ProjectBestand;
import nl.rivm.screenit.model.project.ProjectInactiefReden;
import nl.rivm.screenit.repository.colon.ColonFITUitslagRepository;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonBaseFITService;
import nl.rivm.screenit.service.colon.ColonStudietestService;
import nl.rivm.screenit.service.colon.ColonUitnodigingService;
import nl.rivm.screenit.service.impl.ProjectUitslagenUploadException;
import nl.rivm.screenit.util.FITTestUtil;
import nl.rivm.screenit.util.ProjectUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class ColonStudietestServiceImpl implements ColonStudietestService
{
	@Lazy
	@Autowired
	private ColonBaseFITService fitService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	@Lazy
	private ClientService clientService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ColonUitnodigingService uitnodigingService;

	@Autowired
	private ColonFITUitslagRepository fitUitslagRepository;

	@Override
	public Boolean studietestHeraanmeldenIndienNodig(IFOBTTest studietest)
	{
		var clientIsHeraangemeld = false;
		var uitnodiging = FITTestUtil.getUitnodiging(studietest);

		if (uitnodiging != null)
		{
			var projectclient = ProjectUtil.getHuidigeProjectClient(uitnodiging.getScreeningRonde().getDossier().getClient(), currentDateSupplier.getDate());
			if (projectclient != null && ProjectUtil.isClientActiefInProject(projectclient, currentDateSupplier.getDate()))
			{
				fitService.bepaalEnSetHeraanmeldenTekstKey(studietest);
				if (studietest.getHeraanmeldenTekstKey() != null)
				{
					fitService.heraanmelden(studietest.getColonScreeningRonde(), currentDateSupplier.getLocalDateTime());
					clientIsHeraangemeld = true;
				}
			}
		}
		return clientIsHeraangemeld;
	}

	@Override
	@Transactional
	public void projectClientInactiverenBijVergelijkendOnderzoek(ColonScreeningRonde screeningRonde)
	{
		var client = screeningRonde.getDossier().getClient();
		var projectClient = ProjectUtil.getHuidigeProjectClient(client, currentDateSupplier.getDate());
		if (projectClient != null
			&& ColonOnderzoeksVariant.VERGELIJKEND.name().equals(ProjectUtil.getParameter(projectClient.getProject(), ProjectParameterKey.COLON_ONDERZOEKSVARIANT)))
		{
			clientService.projectClientInactiveren(projectClient, ProjectInactiefReden.INACTIVATIE_VERGELIJKEND_ONDERZOEK, Bevolkingsonderzoek.COLON);
		}
	}

	@Override
	@Transactional
	public void controleerUitslagenbestandOpFouten(IFOBTTest studietest, ProjectBestand uitslagenbestand) throws ProjectUitslagenUploadException
	{
		geefFoutBijInactiefInProject(uitslagenbestand, studietest);
		geefFoutBijUploadenFIT(studietest);
		geefFoutAlsFITNietVerwerktIs(studietest);
		geefFoutBijAfgerondeRonde(studietest);
		geefFoutAlsOngunstigeUitslagVerstuurdIs(studietest);
		geefFoutBijVerstrekenWachttijd(studietest);
	}

	private void geefFoutBijInactiefInProject(ProjectBestand uitslagenBestand, IFOBTTest studietest) throws ProjectUitslagenUploadException
	{
		var client = studietest.getColonScreeningRonde().getDossier().getClient();
		var projectClient = ProjectUtil.getHuidigeProjectClient(client, currentDateSupplier.getDate());
		if (projectClient == null || uitslagenBestand != null && !projectClient.getProject().equals(uitslagenBestand.getProject()))
		{
			throw new ProjectUitslagenUploadException("De cliënt is niet actief in dit project");
		}
	}

	private void geefFoutAlsFITNietVerwerktIs(IFOBTTest studietest) throws ProjectUitslagenUploadException
	{
		var uitnodiging = FITTestUtil.getUitnodiging(studietest);
		var reguliereTest = uitnodiging.getGekoppeldeTest();

		if (!reguliereTest.getStatus().equals(IFOBTTestStatus.UITGEVOERD))
		{
			if (!fitUitslagRepository.findByBarcodeAndBestandStatusNot(reguliereTest.getBarcode(), IFOBTBestandStatus.VERWERKT).isEmpty())
			{
				throw new ProjectUitslagenUploadException("De FIT/Gold is wel geanalyseerd, maar nog niet verwerkt");
			}
			throw new ProjectUitslagenUploadException("De FIT/Gold is niet ontvangen/geanalyseerd");
		}
	}

	private void geefFoutBijUploadenFIT(IFOBTTest studietest) throws ProjectUitslagenUploadException
	{
		if (!studietest.getType().equals(IFOBTType.STUDIE))
		{
			throw new ProjectUitslagenUploadException("De test is geen studietest");
		}
	}

	private void geefFoutBijAfgerondeRonde(IFOBTTest studietest) throws ProjectUitslagenUploadException
	{
		if (studietest.getColonScreeningRonde().getStatus().equals(ScreeningRondeStatus.AFGEROND))
		{
			throw new ProjectUitslagenUploadException("De ronde van de studietest is afgerond");
		}
	}

	private void geefFoutAlsOngunstigeUitslagVerstuurdIs(IFOBTTest studietest) throws ProjectUitslagenUploadException
	{
		var afspraak = studietest.getColonScreeningRonde().getLaatsteAfspraak();
		var uitnodiging = FITTestUtil.getUitnodiging(studietest);
		var reguliereTest = uitnodiging.getGekoppeldeTest();
		if (FITTestUtil.isGunstig(reguliereTest) && afspraak != null)
		{
			throw new ProjectUitslagenUploadException("De intake afspraak is ingepland, de uitslag van de studietest kan niet gewijzigd worden");
		}
	}

	private void geefFoutBijVerstrekenWachttijd(IFOBTTest studietest) throws ProjectUitslagenUploadException
	{
		var uitnodiging = FITTestUtil.getUitnodiging(studietest);
		if (uitnodiging != null && uitnodiging.getUitgesteldeUitslagDatum() != null && uitnodiging.getUitgesteldeUitslagDatum().before(currentDateSupplier.getDate()))
		{
			throw new ProjectUitslagenUploadException("De wachtperiode is verstreken");
		}
	}

	@Override
	@Transactional
	public void verwerkUitslag(IFOBTTest studietest)
	{
		fitService.setStatus(studietest, IFOBTTestStatus.UITGEVOERD);
		studietest.setVerwerkingsDatum(currentDateSupplier.getDate());

		uitnodigingService.verwijderUitgesteldeUitslagDatum(studietest.getColonUitnodigingExtra());

		hibernateService.saveOrUpdateAll(studietest);
	}
}
