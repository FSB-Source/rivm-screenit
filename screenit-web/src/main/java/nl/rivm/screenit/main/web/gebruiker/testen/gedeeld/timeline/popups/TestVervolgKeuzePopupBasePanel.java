package nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.popups;

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

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.TestVervolgKeuzeOptie;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.apache.commons.beanutils.ConstructorUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.TESTEN,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public abstract class TestVervolgKeuzePopupBasePanel extends GenericPanel<TestVervolgKeuzeOptie>
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(TestVervolgKeuzePopupBasePanel.class);

	private IModel<List<Client>> model;

	public TestVervolgKeuzePopupBasePanel(String id, IModel<TestVervolgKeuzeOptie> keuzeModel, IModel<List<Client>> model)
	{
		super(id, keuzeModel);
		this.model = model;
		add(new Label("gebeurtenis", getModelObject().getNaam()));

		Form<Void> form = new Form<Void>("form");
		form.add(getKeuzeDetailPanel("details", form));
		add(form);
	}

	private Component getKeuzeDetailPanel(String id, Form<Void> form)
	{
		List<Object> params = new ArrayList<>();
		params.add(id);
		params.add(this.model);

		Component comp = new EmptyPanel(id);
		try
		{
			Class<? extends AbstractTestBasePopupPanel> detailPanelClass = (Class<? extends AbstractTestBasePopupPanel>) getModelObject().getDetailClass();
			AbstractTestBasePopupPanel detailPanel = (AbstractTestBasePopupPanel) ConstructorUtils.invokeConstructor(detailPanelClass, params.toArray());
			detailPanel.getOpslaanButton("opslaan", this, form);
			comp = detailPanel;
		}
		catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException | InstantiationException e)
		{
			LOG.error("Fout bij maken van gebeurtenis detailpanel", e);
		}
		return comp;
	}

	public abstract void refreshContainer(AjaxRequestTarget target);
}
