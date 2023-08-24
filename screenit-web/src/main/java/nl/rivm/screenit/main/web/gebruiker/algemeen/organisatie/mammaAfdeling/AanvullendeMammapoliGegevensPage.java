package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.mammaAfdeling;

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

import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.KoppelAanParentOrganisatiePanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatiePaspoortPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Mammapoli;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.IModel;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_MAMMA_MAMMAPOLI_ORG_BEHEER },
	checkScope = true,
	level = ToegangLevel.INSTELLING,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class AanvullendeMammapoliGegevensPage extends AanvullendeMammaAfdelingGegevensPage
{
	private static final long serialVersionUID = 1L;

	public AanvullendeMammapoliGegevensPage()
	{
		Mammapoli mammapoli = (Mammapoli) getCurrentSelectedOrganisatie();
		add(new OrganisatiePaspoortPanel("paspoort", ModelUtil.sModel(mammapoli)));

		final IModel<Mammapoli> model = ModelUtil.cModel(mammapoli);
		setDefaultModel(model);

		Form<Void> form = new ScreenitForm<>("form");
		add(form);

		setAlleenInzien(getCurrentSelectedOrganisatie(), Recht.GEBRUIKER_MAMMA_MAMMAPOLI_ORG_BEHEER);

		form.add(new KoppelAanParentOrganisatiePanel<>("parent", model)).setEnabled(!inzien);
		createSubmit(form, model);
		createAnnuleren(form);
	}

}
