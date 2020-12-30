package nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenMedewerkerOvereenkomst;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class DocumentTemplateTestWrapper implements IDetachable
{
	private static final long serialVersionUID = 1L;

	private final Client client;

	private final ColonIntakeAfspraak intakeAfspraak;

	private final AfgeslotenMedewerkerOvereenkomst overeenkomst;

	private final CervixUitnodiging cervixUitnodiging;

	private final BMHKLaboratorium bmhkLaboratorium;

	private boolean microbioloog = false;

	private boolean fromDBBMHKLAB = false;

	private boolean fromDBINTAKELOCATIE = false;

	private boolean fromDBBKSTANDPLAATS = false;

	private boolean freeTextBKRADIOLOOG = false;

	private IModel<Gebruiker> radioloog1;

	private IModel<Gebruiker> radioloog2;

	public DocumentTemplateTestWrapper()
	{
		client = DocumentTemplateTestUtil.getDefaultFilledClient();
		intakeAfspraak = DocumentTemplateTestUtil.getDefaultFilledColonIntakeAfspraak();
		overeenkomst = DocumentTemplateTestUtil.getDefaultFilledOvereenkomst();
		cervixUitnodiging = DocumentTemplateTestUtil.getDefaultCervixUitnodiging();
		client.setCervixDossier(cervixUitnodiging.getScreeningRonde().getDossier());
		cervixUitnodiging.getScreeningRonde().getDossier().setClient(client);
		bmhkLaboratorium = DocumentTemplateTestUtil.getDefaultBmhkLaboratorium();
		cervixUitnodiging.getMonster().setLaboratorium(bmhkLaboratorium);
		client.getPersoon().getGbaAdres().getGbaGemeente().setBmhkLaboratorium(bmhkLaboratorium);
	}

	public boolean isFromDBBMHKLAB()
	{
		return fromDBBMHKLAB;
	}

	public void setFromDBBMHKLAB(boolean fromDBBMHKLAB)
	{
		this.fromDBBMHKLAB = fromDBBMHKLAB;
	}

	public boolean isFromDBINTAKELOCATIE()
	{
		return fromDBINTAKELOCATIE;
	}

	public void setFromDBINTAKELOCATIE(boolean fromDBINTAKELOCATIE)
	{
		this.fromDBINTAKELOCATIE = fromDBINTAKELOCATIE;
	}

	public boolean isFromDBBKSTANDPLAATS()
	{
		return fromDBBKSTANDPLAATS;
	}

	public void setFromDBBKSTANDPLAATS(boolean fromDBBKSTANDPLAATS)
	{
		this.fromDBBKSTANDPLAATS = fromDBBKSTANDPLAATS;
	}

	public boolean isFreeTextBKRADIOLOOG()
	{
		return freeTextBKRADIOLOOG;
	}

	public void setFreeTextBKRADIOLOOG(boolean freeTextBKRADIOLOOG)
	{
		this.freeTextBKRADIOLOOG = freeTextBKRADIOLOOG;
	}

	public Client getClient()
	{
		return client;
	}

	public ColonIntakeAfspraak getIntakeAfspraak()
	{
		return intakeAfspraak;
	}

	public AfgeslotenMedewerkerOvereenkomst getOvereenkomst()
	{
		return overeenkomst;
	}

	public CervixUitnodiging getCervixUitnodiging()
	{
		return cervixUitnodiging;
	}

	public BMHKLaboratorium getBmhkLaboratorium()
	{
		return bmhkLaboratorium;
	}

	public void cloneIntakeLocatie(ColoscopieCentrum dbIntakeLocatie)
	{
		ColoscopieCentrum intakeLocatie = intakeAfspraak.getLocation().getColoscopieCentrum();
		intakeLocatie.setNaam(dbIntakeLocatie.getNaam());
		intakeLocatie.setLocatieBeschrijving(dbIntakeLocatie.getLocatieBeschrijving());
		intakeLocatie.getAfspraakDefinities().get(0).setDuurAfspraakInMinuten(dbIntakeLocatie.getAfspraakDefinities().get(0).getDuurAfspraakInMinuten());
		intakeLocatie.setEmail(dbIntakeLocatie.getEmail());
		intakeLocatie.setWebsite(dbIntakeLocatie.getWebsite());
		intakeLocatie.setTelefoon(dbIntakeLocatie.getTelefoon());
		intakeLocatie.setFax(dbIntakeLocatie.getFax());
		if (dbIntakeLocatie.getAdressen().size() > 0)
		{
			intakeLocatie.getAdressen().get(0).setStraat(dbIntakeLocatie.getAdressen().get(0).getStraat());
			intakeLocatie.getAdressen().get(0).setHuisnummer(dbIntakeLocatie.getAdressen().get(0).getHuisnummer());
			intakeLocatie.getAdressen().get(0).setHuisnummerToevoeging(dbIntakeLocatie.getAdressen().get(0).getHuisnummerToevoeging());
			intakeLocatie.getAdressen().get(0).setPostcode(dbIntakeLocatie.getAdressen().get(0).getPostcode());
			intakeLocatie.getAdressen().get(0).setPlaats(dbIntakeLocatie.getAdressen().get(0).getPlaats());
			if (dbIntakeLocatie.getAdressen().size() > 1)
			{
				intakeLocatie.getAdressen().get(1).setHuisnummer(dbIntakeLocatie.getAdressen().get(1).getHuisnummer());
				intakeLocatie.getAdressen().get(1).setPostcode(dbIntakeLocatie.getAdressen().get(1).getPostcode());
				intakeLocatie.getAdressen().get(1).setPlaats(dbIntakeLocatie.getAdressen().get(1).getPlaats());
			}
			else
			{
				intakeLocatie.getAdressen().get(1).setHuisnummer(dbIntakeLocatie.getAdressen().get(0).getHuisnummer());
				intakeLocatie.getAdressen().get(1).setPostcode(dbIntakeLocatie.getAdressen().get(0).getPostcode());
				intakeLocatie.getAdressen().get(1).setPlaats(dbIntakeLocatie.getAdressen().get(0).getPlaats());
			}
		}
	}

	public boolean isMicrobioloog()
	{
		return microbioloog;
	}

	public void setMicrobioloog(boolean microbioloog)
	{
		this.microbioloog = microbioloog;
	}

	public Gebruiker getRadioloog1()
	{
		if (isFreeTextBKRADIOLOOG())
		{
			return MammaScreeningRondeUtil.getLaatsteBeoordelingVanLaatsteOnderzoek(client).getEersteLezing().getBeoordelaar().getMedewerker();
		}
		return ModelUtil.nullSafeGet(radioloog1);
	}

	public void setRadioloog1(Gebruiker radioloog1)
	{
		this.radioloog1 = ModelUtil.sModel(radioloog1);
	}

	public Gebruiker getRadioloog2()
	{
		if (isFreeTextBKRADIOLOOG())
		{
			return MammaScreeningRondeUtil.getLaatsteBeoordelingVanLaatsteOnderzoek(client).getTweedeLezing().getBeoordelaar().getMedewerker();
		}
		return ModelUtil.nullSafeGet(radioloog2);
	}

	public void setRadioloog2(Gebruiker radioloog2)
	{
		this.radioloog2 = ModelUtil.sModel(radioloog2);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(radioloog1);
		ModelUtil.nullSafeDetach(radioloog2);
	}
}
