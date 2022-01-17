package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public abstract class AbstractBEAccordionPanel<T> extends GenericPanel<T>
{

	private IModel<String> title;

	protected WebMarkupContainer panelContainer;

	private WebMarkupContainer panelParentContainer;

	protected BootstrapDialog dialog;

	private int panelSize;

	private boolean ingeklapt = false;

	private String namePostfixCssClass;

	private WebMarkupContainer panelSizeDiv;

	private boolean useLazyLoading = false;

	public AbstractBEAccordionPanel(String id, IModel<T> model, IModel<String> title, int panelSize)
	{
		super(id, model);
		this.title = title;
		this.panelSize = panelSize;
	}

	public AbstractBEAccordionPanel(String id, IModel<T> model, int panelSize)
	{
		super(id, model);
		this.panelSize = panelSize;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		createPanelContainer();
		createDialog();
	}

	private void createPanelContainer()
	{
		panelSizeDiv = new WebMarkupContainer("panelSize");
		panelSizeDiv.setOutputMarkupId(true);
		add(panelSizeDiv);
		setPanelSize(panelSize);

		panelParentContainer = new WebMarkupContainer("panelParentContainer");
		panelParentContainer.setOutputMarkupId(true);
		panelSizeDiv.add(panelParentContainer);

		panelContainer = new WebMarkupContainer("panelContainer");
		panelContainer.setOutputMarkupId(true);
		panelParentContainer.add(panelContainer);
		WebMarkupContainer collapseLink;
		if (useLazyLoading)
		{
			collapseLink = new IndicatingAjaxLink<Void>("collapseLink")
			{
				@Override
				public void onClick(AjaxRequestTarget target)
				{
					lazyLoadContent(target);
				}
			};
		}
		else
		{
			collapseLink = new WebMarkupContainer("collapseLink");
		}
		collapseLink.add(new AttributeAppender("data-parent", new Model<>("#" + getMarkupId())));
		collapseLink.add(new AttributeAppender("href", new Model<>("#" + panelParentContainer.getMarkupId())));
		panelSizeDiv.add(collapseLink);

		collapseLink.add(new Label("collapseName", title));

		final WebMarkupContainer namePostfix = new WebMarkupContainer("namePostfix");
		if (this.namePostfixCssClass != null)
		{
			namePostfix.add(new AttributeModifier("class", this.namePostfixCssClass));
		}
		collapseLink.add(namePostfix);

		checkPanelIngeklapt();
	}

	protected void lazyLoadContent(AjaxRequestTarget target)
	{
		if (!useLazyLoading)
		{
			throw new IllegalStateException("On click lazy load van de accordion content, override this method!");
		}
	}

	private void checkPanelIngeklapt()
	{
		if (!ingeklapt)
		{
			panelParentContainer.add(new AttributeAppender("class", new Model<>("in"), " "));
		}
	}

	private void createDialog()
	{
		dialog = new BootstrapDialog("dialog")
		{
			@Override
			public boolean fade()
			{
				return false;
			}
		};
		dialog.setOutputMarkupId(true);
		add(dialog);
	}

	public int getPanelSize()
	{
		return panelSize;
	}

	protected void setPanelSize(int panelSize)
	{
		if (panelSize > 0 && panelSize <= 12)
		{
			this.panelSize = panelSize;
			panelSizeDiv.add(new AttributeModifier("class", Model.of("span" + panelSize)));
		}
		else
		{
			throw new IllegalStateException("Panel is te groot");
		}
	}

	public IModel<String> getTitle()
	{
		return title;
	}

	public void setTitle(IModel<String> title)
	{
		this.title = title;
	}

	public void setNamePostfixCssClass(String namePostfixCssClass)
	{
		this.namePostfixCssClass = namePostfixCssClass;
	}

	public boolean isIngeklapt()
	{
		return ingeklapt;
	}

	public void setIngeklapt(boolean ingeklapt)
	{
		this.ingeklapt = ingeklapt;
	}

	protected void refreshPanel(AjaxRequestTarget target)
	{
		checkPanelIngeklapt();
		target.add(panelParentContainer);
		target.add(panelSizeDiv);
	}

	public boolean isUseLazyLoading()
	{
		return useLazyLoading;
	}

	public void setUseLazyLoading(boolean useLazyLoading)
	{
		this.useLazyLoading = useLazyLoading;
	}
}
