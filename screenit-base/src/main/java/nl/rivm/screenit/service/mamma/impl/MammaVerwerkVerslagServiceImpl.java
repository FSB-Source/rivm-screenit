package nl.rivm.screenit.service.mamma.impl;

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

import java.util.Date;
import java.util.List;
import java.util.Objects;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpFollowupPa;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpMonstermateriaal;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpPathologieMedischeObservatie;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpPtnmEnGradering;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpVerrichting;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpVerslagContent;
import nl.rivm.screenit.service.mamma.MammaBaseFollowUpService;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.rivm.screenit.service.mamma.MammaVerwerkVerslagService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaVerwerkVerslagServiceImpl implements MammaVerwerkVerslagService
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaBaseScreeningrondeService screeningrondeService;

	@Autowired
	private MammaBaseFollowUpService followUpService;

	@Override
	public void verwerkVerslagInDossier(MammaFollowUpVerslag verslag)
	{
		hibernateService.saveOrUpdate(verslag);
		MammaScreeningRonde screeningRonde = verslag.getScreeningRonde();
		screeningRonde.getFollowUpVerslagen().add(verslag);

		hibernateService.saveOrUpdate(screeningRonde);
		followUpService.refreshUpdateFollowUpConclusie(screeningRonde.getDossier());
	}

	@Override
	public void verwerkImportVerslagInDossier(MammaFollowUpVerslag nieuwVerslag)
	{
		List<MammaFollowUpVerslag> verslagen = nieuwVerslag.getScreeningRonde().getFollowUpVerslagen();
		for (MammaFollowUpVerslag verslag : verslagen)
		{
			if (isZelfdeVerslag(nieuwVerslag, verslag))
			{
				if (!isIdentiekVerslag(nieuwVerslag, verslag))
				{
					verwerkVerslagInDossier(nieuwVerslag);
					verslagen.remove(verslag);
					hibernateService.delete(verslag);
				}
				return;
			}
		}
		verwerkVerslagInDossier(nieuwVerslag);
	}

	private boolean isZelfdeVerslag(MammaFollowUpVerslag nieuwVerslag, MammaFollowUpVerslag oudVerslag)
	{
		if (VerslagStatus.IN_BEWERKING == oudVerslag.getStatus())
		{
			return false;
		}
		MammaFollowUpVerslagContent nieuwVerslagContent = nieuwVerslag.getVerslagContent();
		MammaFollowUpVerslagContent oudVerslagContent = oudVerslag.getVerslagContent();

		MammaFollowUpPathologieMedischeObservatie nieuwPathologieMedischeObservatie = nieuwVerslagContent.getPathologieMedischeObservatie();
		MammaFollowUpPathologieMedischeObservatie oudPathologieMedischeObservatie = oudVerslagContent.getPathologieMedischeObservatie();

		MammaFollowUpFollowupPa oudFollowupPa = oudVerslagContent.getFollowupPa().get(0);
		MammaFollowUpFollowupPa nieuwFollowupPa = nieuwVerslagContent.getFollowupPa().get(0);

		MammaFollowUpMonstermateriaal nieuwMonstermateriaal = nieuwFollowupPa.getMonstermateriaal();
		MammaFollowUpMonstermateriaal oudMonstermateriaal = oudFollowupPa.getMonstermateriaal();

		return Objects.equals(nieuwVerslag.getType(), oudVerslag.getType())
			&& Objects.equals(DateUtil.toLocalDate(nieuwPathologieMedischeObservatie.getDatumAutorisatieUitslag()),
			DateUtil.toLocalDate(oudPathologieMedischeObservatie.getDatumAutorisatieUitslag()))
			&& Objects.equals(nieuwMonstermateriaal.getVerkrijgingswijze(), oudMonstermateriaal.getVerkrijgingswijze())
			&& Objects.equals(nieuwMonstermateriaal.getZijdigheid(), oudMonstermateriaal.getZijdigheid());
	}

	private boolean isIdentiekVerslag(MammaFollowUpVerslag nieuwVerslag, MammaFollowUpVerslag oudVerslag)
	{
		MammaFollowUpVerslagContent nieuwVerslagContent = nieuwVerslag.getVerslagContent();
		MammaFollowUpVerslagContent oudVerslagContent = oudVerslag.getVerslagContent();

		MammaFollowUpPathologieMedischeObservatie nieuwPathologieMedischeObservatie = nieuwVerslagContent.getPathologieMedischeObservatie();
		MammaFollowUpPathologieMedischeObservatie oudPathologieMedischeObservatie = oudVerslagContent.getPathologieMedischeObservatie();

		MammaFollowUpFollowupPa oudFollowupPa = oudVerslagContent.getFollowupPa().get(0);
		MammaFollowUpFollowupPa nieuwFollowupPa = nieuwVerslagContent.getFollowupPa().get(0);

		MammaFollowUpMonstermateriaal nieuwMonstermateriaal = nieuwFollowupPa.getMonstermateriaal();
		MammaFollowUpMonstermateriaal oudMonstermateriaal = oudFollowupPa.getMonstermateriaal();

		MammaFollowUpPtnmEnGradering nieuwPtnmEnGradering = nieuwFollowupPa.getPtnmEnGradering();
		MammaFollowUpPtnmEnGradering oudPtnmEnGradering = oudFollowupPa.getPtnmEnGradering();

		MammaFollowUpVerrichting nieuwVerrichting = nieuwVerslagContent.getVerrichting();
		MammaFollowUpVerrichting oudVerrichting = oudVerslagContent.getVerrichting();

		return Objects.equals(nieuwVerslag.getStatus(), oudVerslag.getStatus())
			&& Objects.equals(nieuwVerslag.getType(), oudVerslag.getType())
			&& Objects.equals(DateUtil.toLocalDate(nieuwVerslag.getDatumOnderzoek()), DateUtil.toLocalDate(oudVerslag.getDatumOnderzoek()))
			&& Objects.equals(nieuwMonstermateriaal.getVerkrijgingswijze(), oudMonstermateriaal.getVerkrijgingswijze())
			&& Objects.equals(nieuwMonstermateriaal.getZijdigheid(), oudMonstermateriaal.getZijdigheid())
			&& Objects.equals(nieuwMonstermateriaal.getLocatietopologie(), oudMonstermateriaal.getLocatietopologie())
			&& Objects.equals(nieuwMonstermateriaal.getLocatieuren(), oudMonstermateriaal.getLocatieuren())
			&& Objects.equals(nieuwFollowupPa.getCclassificatiePunctie(), oudFollowupPa.getCclassificatiePunctie())
			&& Objects.equals(nieuwFollowupPa.getOestrogeenReceptorStatus(), oudFollowupPa.getOestrogeenReceptorStatus())
			&& Objects.equals(nieuwFollowupPa.getProgesteronReceptorStatus(), oudFollowupPa.getProgesteronReceptorStatus())
			&& Objects.equals(nieuwFollowupPa.getHer2Status(), oudFollowupPa.getHer2Status())
			&& Objects.equals(nieuwFollowupPa.getBclassificatieOpMammabiopt(), oudFollowupPa.getBclassificatieOpMammabiopt())
			&& Objects.equals(nieuwFollowupPa.getMaligniteitsgraad(), oudFollowupPa.getMaligniteitsgraad())
			&& isIdentiekePtnm(nieuwPtnmEnGradering, oudPtnmEnGradering)
			&& Objects.equals(nieuwFollowupPa.getTypeInvasieveTumorwhoOverige(), oudFollowupPa.getTypeInvasieveTumorwhoOverige())
			&& Objects.equals(nieuwFollowupPa.getGraderingDcis(), oudFollowupPa.getGraderingDcis())
			&& CollectionUtils.isEqualCollection(nieuwFollowupPa.getTypeNietEenduidigBenigneLaesies(), oudFollowupPa.getTypeNietEenduidigBenigneLaesies())
			&& CollectionUtils.isEqualCollection(nieuwFollowupPa.getTypeEenduidigBenigneLaesies(), oudFollowupPa.getTypeEenduidigBenigneLaesies())
			&& CollectionUtils.isEqualCollection(nieuwFollowupPa.getTypeCis(), oudFollowupPa.getTypeCis())
			&& Objects.equals(DateUtil.toLocalDate(nieuwPathologieMedischeObservatie.getDatumOntvangstMateriaal()),
			DateUtil.toLocalDate(oudPathologieMedischeObservatie.getDatumOntvangstMateriaal()))
			&& Objects.equals(nieuwPathologieMedischeObservatie.getTnummerLaboratorium(), oudPathologieMedischeObservatie.getTnummerLaboratorium())
			&& Objects.equals(nieuwPathologieMedischeObservatie.getVersieProtocol(), oudPathologieMedischeObservatie.getVersieProtocol())
			&& Objects.equals(nieuwVerrichting.getAanvangVerrichting(), oudVerrichting.getAanvangVerrichting())
			&& Objects.equals(nieuwVerrichting.getEindeVerrichting(), oudVerrichting.getEindeVerrichting());
	}

	private boolean isIdentiekePtnm(MammaFollowUpPtnmEnGradering nieuwPtnmEnGradering, MammaFollowUpPtnmEnGradering oudPtnmEnGradering)
	{
		if (nieuwPtnmEnGradering == null && oudPtnmEnGradering == null)
		{
			return true;
		}
		if (nieuwPtnmEnGradering == null || oudPtnmEnGradering == null)
		{
			return false;
		}

		return Objects.equals(nieuwPtnmEnGradering.getPt(), oudPtnmEnGradering.getPt())
			&& Objects.equals(nieuwPtnmEnGradering.getPn(), oudPtnmEnGradering.getPn())
			&& Objects.equals(nieuwPtnmEnGradering.getPm(), oudPtnmEnGradering.getPm())
			&& Objects.equals(nieuwPtnmEnGradering.getPtnmbreastGradering(), oudPtnmEnGradering.getPtnmbreastGradering());

	}

	@Override
	public MammaScreeningRonde getValideScreeningsRonde(Client client, Date onderzoeksdatum)
	{
		return screeningrondeService.getLaatsteScreeningRondeMetUitslag(client, onderzoeksdatum);
	}

	@Override
	public void onAfterVerwerkVerslagContent(MammaFollowUpVerslag verslag)
	{
		verslag.setDatumOnderzoek(verslag.getVerslagContent().getPathologieMedischeObservatie().getDatumAutorisatieUitslag());
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public void valideerVerslagVoorAfronden(MammaFollowUpVerslag verslag, InstellingGebruiker instellingGebruiker)
	{
		for (MammaFollowUpFollowupPa followupPa : verslag.getVerslagContent().getFollowupPa())
		{
			boolean heeftLocatietopologie = followupPa.getMonstermateriaal().getLocatietopologie() != null;
			boolean heeftLocatieuren = followupPa.getMonstermateriaal().getLocatieuren() != null;
			if (heeftLocatietopologie == heeftLocatieuren)
			{
				throw new IllegalStateException("error.followup.locatieuren.locatietopologie");
			}
		}
	}

}
