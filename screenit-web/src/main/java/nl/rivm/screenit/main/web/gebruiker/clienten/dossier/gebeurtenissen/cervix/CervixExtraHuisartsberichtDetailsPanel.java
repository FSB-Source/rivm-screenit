package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.cervix;

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

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.AbstractGebeurtenisDetailPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.controleren.CervixHuisartsLocatiePanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.IModel;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_CLIENT_SR_HUISARTSBERICHT_DETAILS,
	bevolkingsonderzoekScopes = Bevolkingsonderzoek.CERVIX)
public class CervixExtraHuisartsberichtDetailsPanel extends AbstractGebeurtenisDetailPanel
{
	private static final long serialVersionUID = 1L;

	public CervixExtraHuisartsberichtDetailsPanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, model);

		add(new Label("huisartsBericht.extraHuisartsLocatie.zorgmailklantnummer"));
		add(new Label("huisartsBericht.berichtType.naam"));
		add(new Label("huisartsBericht.aanmaakDatum"));
		add(DateLabel.forDatePattern("huisartsBericht.extraHuisartsLocatieVerstuurdDatum", "dd-MM-yyyy HH:mm"));

		CervixHuisartsBericht huisartsBericht = getModelObject().getHuisartsBericht();
		WebMarkupContainer huisartsLocatiePanel = maakInzienHuisartsLocatiePanel(huisartsBericht.getExtraHuisartsLocatie());
		add(huisartsLocatiePanel);
	}

	private CervixHuisartsLocatiePanel maakInzienHuisartsLocatiePanel(CervixHuisartsLocatie huisartsLocatie)
	{
		return new CervixHuisartsLocatiePanel("huisartsLocatieContainer", ModelUtil.cRModel(huisartsLocatie), false)
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void wijzigHuisartsLocatie(AjaxRequestTarget target)
			{
			}
		};
	}
}
