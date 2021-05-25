package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie;

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

import static nl.rivm.screenit.main.web.base.BasePage.markeerFormulierenOpgeslagen;

import nl.rivm.screenit.main.model.Parameterisatie;
import nl.rivm.screenit.main.service.ParameterisatieService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenITDoubleConverter;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.convert.IConverter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class BasePrimaireParametersPanel extends GenericPanel<Parameterisatie>
{
	private static final Logger LOG = LoggerFactory.getLogger(BasePrimaireParametersPanel.class);

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ParameterisatieService parameterisatieService;

	private Parameterisatie oudParameterObject;

	public BasePrimaireParametersPanel(String id, IModel<Parameterisatie> model)
	{
		super(id, new ParameterisatiePropertyModel<>(model));
		setOudParametersObject(model.getObject());

		boolean magAanpassen = magAanpassen();

		ToegangLevel level = getToegangsLevel();

		Form<Parameterisatie> form = createAndGetForm();
		form.setEnabled(magAanpassen);

		Component opslaanLink = createAndGetOpslaanLink();
		opslaanLink.setVisible(magAanpassen && ToegangLevel.LANDELIJK.equals(level));
		form.add(opslaanLink);
		add(form);
	}

	protected ToegangLevel getToegangsLevel()
	{
		return ScreenitSession.get().getToegangsLevel(Actie.AANPASSEN, Recht.GEBRUIKER_BEHEER_PARAMETERISATIE);
	}

	protected boolean magAanpassen()
	{
		return ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BEHEER_PARAMETERISATIE, Actie.AANPASSEN);
	}

	protected abstract Form<Parameterisatie> createAndGetForm();

	protected abstract Component createAndGetOpslaanLink();

	protected void addTextAreaField(Form<Parameterisatie> form, String id)
	{
		final TextArea<String> tekstField = new TextArea<String>(id)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public String getMarkupId()
			{
				return id;
			}

		};
		tekstField.setOutputMarkupId(true);
		tekstField.setRequired(true);
		form.add(tekstField);
	}

	protected TextField<Double> createDoubleTextField(String id)
	{
		return new TextField<Double>(id, Double.class)
		{
			private static final long serialVersionUID = 1L;

			@Override
			public <C> IConverter<C> getConverter(Class<C> type)
			{
				if (type.equals(Double.class))
				{
					return (IConverter<C>) new ScreenITDoubleConverter();
				}
				return super.getConverter(type);
			}
		};
	}

	protected void opslaan(AjaxRequestTarget target, Bevolkingsonderzoek... bvo)
	{
		Parameterisatie parameterisatie = getModelObject();
		parameterisatieService.saveParameters(ScreenitSession.get().getLoggedInAccount(), parameterisatie, oudParameterObject, bvo);
		setOudParametersObject(parameterisatie);
		markeerFormulierenOpgeslagen(target);
	}

	protected Parameterisatie getOudParameterObject()
	{
		return oudParameterObject;
	}

	private void setOudParametersObject(Parameterisatie parameterisatie)
	{
		oudParameterObject = parameterisatie.clone();
	}

	@Override
	protected void detachModel()
	{
		ModelUtil.nullSafeDetach(oudParameterObject);
	}
}
