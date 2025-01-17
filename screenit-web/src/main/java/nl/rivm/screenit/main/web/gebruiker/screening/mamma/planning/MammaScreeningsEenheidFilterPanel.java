package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.service.InstellingService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class MammaScreeningsEenheidFilterPanel extends GenericPanel<MammaScreeningsEenheidFilter>
{

	@SpringBean
	private InstellingService instellingService;

	public MammaScreeningsEenheidFilterPanel(String id)
	{
		super(id);

		ScreeningOrganisatie sessionSO = ScreenitSession.get().getScreeningOrganisatie();

		IModel<MammaScreeningsEenheidFilter> zoekObjectModel = (IModel<MammaScreeningsEenheidFilter>) ScreenitSession.get()
			.getZoekObject(MammaScreeningsEenheidFilterPanel.class);
		if (zoekObjectModel == null)
		{
			MammaScreeningsEenheidFilter zoekObject = new MammaScreeningsEenheidFilter();
			zoekObject.setRegio(sessionSO);
			zoekObject.setActief(true);
			zoekObjectModel = new CompoundPropertyModel<>(zoekObject);
			ScreenitSession.get().setZoekObject(MammaScreeningsEenheidFilterPanel.class, zoekObjectModel);
		}
		setModel(zoekObjectModel);

		Form<MammaScreeningsEenheidFilter> form = new Form<>("form");
		add(form);

		form.add(new TextField<>("screeningsEenheid.naam"));

		ScreenitDropdown<ScreeningOrganisatie> regio = new ScreenitDropdown<>("regio",
			ModelUtil.listRModel(instellingService.getActieveInstellingen(ScreeningOrganisatie.class), true), new ChoiceRenderer<>("naam"));
		regio.setVisible(sessionSO == null);
		regio.setNullValid(true);
		form.add(regio);

		AjaxSubmitLink zoekenBtn = new AjaxSubmitLink("zoeken")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				target.add(zoeken(getModel()));
				ScreenitSession.get().setZoekObject(MammaScreeningsEenheidFilterPanel.class, getModel());
			}
		};
		form.setDefaultButton(zoekenBtn);
		form.add(zoekenBtn);
	}

	protected abstract WebMarkupContainer zoeken(IModel<MammaScreeningsEenheidFilter> filter);

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(getModel());
	}
}
