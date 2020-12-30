
package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.colon;

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

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.AbstractGebeurtenisDetailPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.BooleanLabel;

import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.model.IModel;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_CLIENT_SR_CONCLUSIE,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class ConclusieInzienPanel extends AbstractGebeurtenisDetailPanel
{
	public ConclusieInzienPanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, model);

		ColonIntakeAfspraak afspraak = ModelUtil.nullSafeGet(model).getAfspraak();
		ColonConclusie conclusie = afspraak.getConclusie();

		add(new EnumLabel<ColonConclusieType>("afspraak.conclusie.type"));
		add(DateLabel.forDatePattern("afspraak.conclusie.datum", "dd-MM-yyyy HH:mm"));
		add(new Label("afspraak.conclusie.instellingGebruiker.medewerker.naamVolledig"));
		add(new BooleanLabel("afspraak.bezwaar").setVisible(Boolean.TRUE.equals(afspraak.getBezwaar())));

		WebMarkupContainer newContainer = new EmptyPanel("container");
		if (conclusie.getType() != null)
		{
			switch (conclusie.getType())
			{
			case COLOSCOPIE:
				newContainer = new DatumNaarColoscopiePanel("container");
				break;
			case GEEN_VERVOLGONDERZOEK:
				newContainer = new RedenGeenOnderzoekPanel("container");
				break;
			default:
				break;
			}
		}
		add(newContainer);
	}

	private class RedenGeenOnderzoekPanel extends Fragment
	{

		public RedenGeenOnderzoekPanel(String id)
		{
			super(id, "redenExclusieFragment", ConclusieInzienPanel.this);

			add(new EnumLabel<>("afspraak.conclusie.geenOnderzoekReden"));
		}
	}

	private class DatumNaarColoscopiePanel extends Fragment
	{

		public DatumNaarColoscopiePanel(String id)
		{
			super(id, "coloscopieFragement", ConclusieInzienPanel.this);

			add(DateLabel.forDatePattern("afspraak.conclusie.datumColoscopie", "dd-MM-yyyy"));

		}
	}
}
