package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.AbstractGebeurtenisDetailPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.controleren.SpherionResourceLink;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.controleren.SpherionViewerContainer;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.IModel;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_CLIENT_SR_CERVIX_INZIEN_BMHK_FORMULIER,
	bevolkingsonderzoekScopes = Bevolkingsonderzoek.CERVIX)
public class CervixLabformulierInzienPanel extends AbstractGebeurtenisDetailPanel
{

	public CervixLabformulierInzienPanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		CervixUitnodiging uitnodiging = (CervixUitnodiging) HibernateHelper.deproxy(getModelObject().getUitnodiging());

		CervixUitstrijkje uitstrijkje = CervixMonsterUtil.getUitstrijkje(uitnodiging.getMonster());
		CervixLabformulier labformulier = uitstrijkje.getLabformulier();
		String objid = labformulier.getObjid();
		add(new Label("monsterId", uitstrijkje.getMonsterId() + ""));
		add(new Label("huisarts", getHuisartsInfo(labformulier)));
		add(new EnumLabel<>("status", labformulier.getStatus()));

		boolean magHpvMinInzien = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_INZIEN_FORMULIER_NA_HPVMIN, Actie.INZIEN);

		if (uitstrijkje.getLabformulier().getDatumGewist() == null || magHpvMinInzien)
		{
			add(new SpherionResourceLink("download", objid));
			add(new SpherionViewerContainer("labformulier", objid));
		}
		else
		{
			add(new Label("download", ""));
			add(new Label("labformulier", "Voor dit monster is een negatieve HPV uitslag ontvangen, waardoor het aanvraagformulier niet (meer) kan worden ingezien"));
		}
	}

	private String getHuisartsInfo(CervixLabformulier labformulier)
	{
		String huistartsInfo = "";
		CervixHuisarts huisarts = null;
		if (labformulier.getHuisartsLocatie() != null)
		{
			huisarts = labformulier.getHuisartsLocatie().getHuisarts();
		}
		if (huisarts != null)
		{
			huistartsInfo = huisarts.getOrganisatieMedewerkers().get(0).getMedewerker().getAchternaam();
			huistartsInfo += " te " + labformulier.getHuisartsLocatie().getLocatieAdres().getWoonplaats().getNaam();
		}
		return huistartsInfo;
	}

}
